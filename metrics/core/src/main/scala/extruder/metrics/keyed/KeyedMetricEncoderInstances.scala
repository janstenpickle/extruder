package extruder.metrics.keyed

import cats.Applicative
import extruder.core.Transform
import extruder.data.PathElement
import extruder.metrics.data._
import extruder.metrics.{MetricEncoderInstances, MetricSettings}

trait KeyedMetricEncoderInstances extends MetricEncoderInstances {
  protected def buildMetrics[F[_], S <: MetricSettings](settings: S, inter: Metrics)(
    implicit F: Applicative[F]
  ): F[Iterable[KeyedMetric]] =
    F.pure(
      inter
        .foldLeft(Map.empty[(String, MetricType), Numbers]) {
          case (acc, (k, v)) =>
            val keys = k match {
              case key: SimpleMetricKey =>
                Map(settings.pathToString(key.path :+ key.name) -> key.metricType.getOrElse(settings.defaultMetricType))
              case key: DimensionalMetricKey =>
                val metricType = key.metricType.getOrElse(settings.defaultMetricType)
                key.dimensions.map {
                  case (labelName, labelValue) =>
                    settings.pathToString(key.path ++ List(key.name, labelName, labelValue)) -> metricType
                }
            }

            acc ++ keys.map { key =>
              key -> acc.get(key).fold(v)(Numbers.add(v, _))
            }
        }
        .map { case ((k, t), v) => KeyedMetric(k, t, v) }
        .toList
    )

  implicit def keyedMetricsTransform[F[_]: Applicative, S <: MetricSettings]: Transform[F, S, Metrics, Iterable[
    KeyedMetric
  ]] =
    new Transform[F, S, Metrics, Iterable[KeyedMetric]] {
      override def run(namespace: List[PathElement], settings: S, inputData: Metrics): F[Iterable[KeyedMetric]] =
        buildMetrics[F, S](settings, inputData)
    }
}
