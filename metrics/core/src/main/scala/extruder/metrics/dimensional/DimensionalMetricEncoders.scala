package extruder.metrics.dimensional

import cats.data.NonEmptyList
import extruder.core.{EncoderRefute, Show}
import extruder.metrics.MetricEncoders
import extruder.metrics.data._
import shapeless.ops.hlist.{Length, Mapper, Repeat, Take}
import shapeless.{<:!<, =:!=, Generic, HList, Nat, Poly1, Refute}

import scala.concurrent.duration.FiniteDuration

trait DimensionalMetricEncoders extends MetricEncoders {
  val metricTypeName = "metric_type"

  override type EncRefute[T] = DimensionalMetricEncoderRefute[T]

  def labelTransform(value: String): String

  implicit def resetNamespaceEncoder[F[_]: Eff, T](implicit enc: Enc[F, T]): Enc[F, ResetNamespace[T]] =
    mkEncoder[F, ResetNamespace[T]] { (path, v) =>
      enc.write(path.lastOption.fold(path)(List(_)), v.value)
    }

  protected def buildMetrics[F[_]](
    namespaceName: Option[String],
    namespace: List[String],
    inter: Metrics,
    defaultLabels: Map[String, String],
    defaultMetricType: MetricType
  )(implicit F: Eff[F], hints: Hint): F[Iterable[DimensionalMetric]] = {
    val defaultDimensionNames = defaultLabels.keySet ++ namespaceName + metricTypeName
    val defaultDimensionValues = defaultLabels.values.toVector

    val metrics =
      inter.metrics
        .flatMap { case (k, v) => calculateMetricDimensions(namespace, defaultMetricType)(k, v) }
        .groupBy(_.key)
        .map {
          case ((name, metricType, labelName), ms) =>
            val dimensions: Set[String] = defaultDimensionNames ++ labelName

            val values = ms.foldLeft(Map.empty[Vector[String], Numbers]) {
              case (acc, metric) =>
                val dimensionValues = (defaultDimensionValues ++ namespaceName
                  .map(_ => metric.namespace.getOrElse("")) :+ metricType.name) ++ metric.labelVal

                acc + (dimensionValues -> acc.get(dimensionValues).fold(metric.value)(Numbers.add(_, metric.value)))
            }

            DimensionalMetric(name, dimensions, metricType, values)
        }

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

  private def calculateDimensions(namespace: List[String], defaultMetricType: MetricType)(key: MetricKey, v: Numbers)(
    implicit hints: Hint
  ): Option[Metric] =
    calculateMetricDimensions(namespace, defaultMetricType)(key, v)


  private def calculateStatusDimensions(
    namespace: List[String]
  )(key: List[String], v: String)(implicit hints: Hint): Option[Metric] =
    (namespace ++ key.name).reverse match {
      case name :: nameTail =>
        Some(SimpleMetric(labelTransform(name), calculateNameSpace(nameTail), MetricType.Status, v))
      case _ => None
    }

  private def calculateMetricDimensions(
    namespace: List[String],
    defaultMetricType: MetricType
  )(key: MetricKey, v: Numbers)(implicit hints: Hint): Option[Metric] =
    (namespace ++ key.name).reverse match {
      case labelValue :: labelName :: name :: nameTail =>
        Some(
          LabelledMetric(
            labelTransform(name),
            calculateNameSpace(nameTail),
            labelTransform(labelName),
            labelValue,
            key.metricType.getOrElse(defaultMetricType),
            v
          )
        )
      case name :: nameTail =>
        Some(
          SimpleMetric(
            labelTransform(name),
            calculateNameSpace(nameTail),
            key.metricType.getOrElse(defaultMetricType),
            v
          )
        )
      case _ => None
    }

  private def calculateNameSpace(nameTail: List[String])(implicit hints: Hint): Option[String] =
    NonEmptyList.fromList(nameTail).map(nel => hints.pathToString(nel.toList))

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

sealed trait Metric {
  def name: String
  def metricType: MetricType
  def value: Numbers
  def namespace: Option[String]
  def labelPair: Option[(String, String)]
  def labelVal: Option[String] = labelPair.map(_._2)

  def key: (String, MetricType, Option[String]) = (name, metricType, labelPair.map(_._1))
}
case class LabelledMetric private[dimensional] (
  name: String,
  namespace: Option[String],
  labelName: String,
  labelValue: String,
  metricType: MetricType,
  value: Numbers
) extends Metric {
  override def labelPair: Option[(String, String)] = Some(labelName -> labelValue)
}
case class SimpleMetric private[dimensional] (
  name: String,
  namespace: Option[String],
  metricType: MetricType,
  value: Numbers
) extends Metric {
  override def labelPair: Option[(String, String)] = None
}

case class DimensionalMetric private[dimensional] (
  name: String,
  labelNames: Set[String],
  metricType: MetricType,
  values: Map[Vector[String], Numbers]
)
