package extruder.metrics.dimensional

import cats.Applicative
import cats.data.NonEmptyList
import extruder.core._
import extruder.metrics.MetricEncoderInstances
import extruder.metrics.data._
import shapeless.{Generic, HList, Refute}

import scala.collection.SortedSet

trait DimensionalMetricEncoderInstances extends MetricEncoderInstances {
  val metricTypeName = "metric_type"

  implicit def resetNamespaceEncoder[F[_], T, S](
    implicit enc: EncoderT[F, S, T, Metrics]
  ): EncoderT[F, S, ResetNamespace[T], Metrics] =
    EncoderT.make[F, S, ResetNamespace[T], Metrics] { (path, settings, v) =>
      enc.write(path.lastOption.fold(path)(List(_)), settings, v.value)
    }

  protected def buildMetrics[F[_], S <: DimensionalMetricSettings](
    namespace: List[String],
    settings: S,
    inter: Metrics
  )(implicit F: Applicative[F], error: ExtruderErrors[F]): F[Iterable[DimensionalMetric]] = {
    val defaultDimensionNames = settings.defaultLabels.keys.toVector
    val defaultDimensionValues = settings.defaultLabels.values.toVector

    val metrics = inter.toList
      .groupBy(metricGrouping(settings, settings.defaultMetricType))
      .map(makeDimensional(namespace, settings, defaultDimensionNames, defaultDimensionValues))

    val metricTypes = metrics
      .foldLeft(Map.empty[String, Set[MetricType]]) { (acc, m) =>
        acc + (m.name -> (acc.getOrElse(m.name, Set.empty) + m.metricType))
      }
      .filter(_._2.size != 1)

    if (metricTypes.isEmpty) F.pure(metrics)
    else {
      val invalid = metricTypes.map { case (name, types) => s"'$name' -> '${types.mkString(", ")}'" }.mkString(", ")
      error.validationFailure(
        s"Multiple metric types for the following metrics, please ensure there is just one: $invalid"
      )
    }
  }

  private def metricGrouping[S <: DimensionalMetricSettings](
    settings: S,
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

  private def makeDimensional[S <: DimensionalMetricSettings](
    namespace: List[String],
    settings: S,
    defaultDimensionNames: Vector[String],
    defaultDimensionValues: Vector[String]
  ): (((String, MetricType, SortedSet[String]), List[(MetricKey, Numbers)])) => DimensionalMetric = {
    case ((name, metricType, dimensions), ms) =>
      val allDimensions: Vector[String] = ((defaultDimensionNames ++ settings.namespaceName) :+ metricTypeName) ++ dimensions.toVector

      val values = ms.foldLeft(Map.empty[Vector[String], Numbers]) {
        case (acc, (key, value)) =>
          val baseDimensionValues: Vector[String] = (defaultDimensionValues ++ settings.namespaceName
            .map(_ => calculateNameSpace(namespace ++ key.path, settings).getOrElse(""))) :+ metricType.name

          val dimensionValues: Vector[String] = key match {
            case _: SimpleMetricKey => baseDimensionValues
            case k: DimensionalMetricKey => baseDimensionValues ++ k.dimensions.toVector.sortBy(_._1).map(_._2)
          }

          acc + (dimensionValues -> acc.get(dimensionValues).fold(value)(Numbers.add(_, value)))
      }

      DimensionalMetric(name, allDimensions, metricType, values)
  }

  private def calculateNameSpace[S <: Settings](nameTail: List[String], settings: S): Option[String] =
    NonEmptyList.fromList(nameTail).map(nel => settings.pathToString(nel.toList))

  implicit def refute[T, Repr <: HList, S](
    implicit gen: Generic.Aux[T, Repr],
    unRefute: Refute[UnRefute[T]]
  ): EncoderTRefute[T, S, Metrics] =
    new EncoderTRefute[T, S, Metrics] {}

  implicit def dimensionalMetricsTransform[F[_]: Applicative: ExtruderErrors, S <: DimensionalMetricSettings]: Transform[
    F,
    S,
    Metrics,
    Iterable[DimensionalMetric]
  ] =
    new Transform[F, S, Metrics, Iterable[DimensionalMetric]] {
      override def run(namespace: List[String], settings: S, inputData: Metrics): F[Iterable[DimensionalMetric]] =
        buildMetrics[F, S](namespace, settings, inputData)
    }
}
