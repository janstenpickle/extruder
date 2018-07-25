package extruder.metrics.dimensional

import cats.data.NonEmptyList
import extruder.core.{EncoderRefute, Show}
import extruder.metrics.{snakeCaseTransformation, MetricEncoders, MetricSettings}
import extruder.metrics.data._
import shapeless.ops.hlist.{Length, Mapper, Repeat, Take}
import shapeless.{<:!<, =:!=, Generic, HList, Nat, Poly1, Refute}

import scala.collection.SortedSet
import scala.concurrent.duration.FiniteDuration

trait DimensionalMetricEncoders extends MetricEncoders {
  val metricTypeName = "metric_type"

  override type EncRefute[T] = DimensionalMetricEncoderRefute[T]
  override type Sett <: DimensionalMetricSettings

  implicit def resetNamespaceEncoder[F[_]: Eff, T](implicit enc: Enc[F, T]): Enc[F, ResetNamespace[T]] =
    mkEncoder[F, ResetNamespace[T]] { (path, settings: Sett, v) =>
      enc.write(path.lastOption.fold(path)(List(_)), settings, v.value)
    }

  protected def buildMetrics[F[_]](
    namespaceName: Option[String],
    namespace: List[String],
    settings: Sett,
    inter: Metrics,
    defaultLabels: Map[String, String],
    defaultMetricType: MetricType
  )(implicit F: Eff[F]): F[Iterable[DimensionalMetric]] = {
    val defaultDimensionNames = defaultLabels.keys.toVector
    val defaultDimensionValues = defaultLabels.values.toVector

    val metrics = inter.toList
      .groupBy(metricGrouping(settings, defaultMetricType))
      .map(makeDimensional(namespaceName, namespace, settings, defaultDimensionNames, defaultDimensionValues))

    val metricTypes = metrics
      .foldLeft(Map.empty[String, Set[MetricType]]) { (acc, m) =>
        acc + (m.name -> (acc.getOrElse(m.name, Set.empty) + m.metricType))
      }
      .filter(_._2.size != 1)

    if (metricTypes.isEmpty) F.pure(metrics)
    else {
      val invalid = metricTypes.map { case (name, types) => s"'$name' -> '${types.mkString(", ")}'" }.mkString(", ")
      F.validationFailure(s"Multiple metric types for the following metrics, please ensure there is just one: $invalid")
    }
  }

  private def metricGrouping(
    settings: Sett,
    defaultMetricType: MetricType
  ): ((MetricKey, Numbers)) => (String, MetricType, SortedSet[String]) = {
    case (SimpleMetricKey(name, _, metricType), _) =>
      (settings.labelTransform(name), metricType.getOrElse(defaultMetricType), SortedSet.empty[String])
    case (DimensionalMetricKey(name, _, dimensions, metricType), _) =>
      (
        settings.labelTransform(name),
        metricType.getOrElse(defaultMetricType),
        SortedSet(dimensions.keys.map(settings.labelTransform).toSeq: _*)
      )
  }

  private def makeDimensional(
    namespaceName: Option[String],
    namespace: List[String],
    settings: Sett,
    defaultDimensionNames: Vector[String],
    defaultDimensionValues: Vector[String]
  ): (((String, MetricType, SortedSet[String]), List[(MetricKey, Numbers)])) => DimensionalMetric = {
    case ((name, metricType, dimensions), ms) =>
      val allDimensions: Vector[String] = ((defaultDimensionNames ++ namespaceName) :+ metricTypeName) ++ dimensions.toVector

      val values = ms.foldLeft(Map.empty[Vector[String], Numbers]) {
        case (acc, (key, value)) =>
          val baseDimensionValues: Vector[String] = (defaultDimensionValues ++ namespaceName
            .map(_ => calculateNameSpace(namespace ++ key.path, settings).getOrElse(""))) :+ metricType.name

          val dimensionValues: Vector[String] = key match {
            case _: SimpleMetricKey => baseDimensionValues
            case k: DimensionalMetricKey => baseDimensionValues ++ k.dimensions.toVector.sortBy(_._1).map(_._2)
          }

          acc + (dimensionValues -> acc.get(dimensionValues).fold(value)(Numbers.add(_, value)))
      }

      DimensionalMetric(name, allDimensions, metricType, values)
  }

  private def calculateNameSpace(nameTail: List[String], settings: Sett): Option[String] =
    NonEmptyList.fromList(nameTail).map(nel => settings.pathToString(nel.toList))

}

trait DimensionalMetricEncoderRefute[T] extends EncoderRefute[T]

object DimensionalMetricEncoderRefute {
  implicit def refute[T, Repr <: HList](
    implicit gen: Generic.Aux[T, Repr],
    unRefute: Refute[UnRefute[T]]
  ): DimensionalMetricEncoderRefute[T] =
    new DimensionalMetricEncoderRefute[T] {}
}

trait UnRefute[T] {}

object UnRefute {
  implicit def unRefuteGeneric[T, Repr <: HList](
    implicit gen: Generic.Aux[T, Repr],
    mapper: Mapper[genericElements.type, Repr],
    refuteLen: Refute[Length.Aux[Repr, Nat._1]]
  ): UnRefute[T] = new UnRefute[T] {}

  object genericElements extends Poly1 {
    implicit def caseGeneric[A](
      implicit gen: Generic[A],
      ev: A <:!< MetricValue[_],
      ev1: A <:!< ResetNamespace[_],
      ev2: A =:!= FiniteDuration
    ): Case.Aux[A, Unit] = at[A](_ => ())

    implicit def caseNonNumericShow[A](
      implicit show: Show[A],
      refute: Refute[Numeric[A]],
      ev: A =:!= FiniteDuration
    ): Case.Aux[A, Unit] =
      at[A](_ => ())
  }

  implicit def unRefuteIdentical[A, Repr <: HList, Len <: Nat, Taken <: HList, Rep <: HList](
    implicit gen: Generic.Aux[A, Repr],
    len: Length.Aux[Repr, Len],
    take: Take.Aux[Repr, Nat._1, Taken],
    rep: Repeat.Aux[Taken, Len, Rep],
    ev: Rep =:= Repr
  ): UnRefute[A] = new UnRefute[A] {}

  implicit def unRefuteMetricValue[A](implicit ev: A <:< MetricValue[_]): UnRefute[A] = new UnRefute[A] {}
}

case class DimensionalMetric private[dimensional] (
  name: String,
  labelNames: Vector[String],
  metricType: MetricType,
  values: Map[Vector[String], Numbers]
)

trait DimensionalMetricSettings extends MetricSettings {
  def labelTransform(value: String): String = snakeCaseTransformation(value)
}
