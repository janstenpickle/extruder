package extruder.metrics.keyed

import extruder.metrics.MetricEncoders
import extruder.metrics.data.{MetricType, Metrics, Numbers}

trait KeyedMetricEncoders extends MetricEncoders {
  protected def buildMetrics[F[_]](
    inter: Metrics,
    defaultMetricType: MetricType
  )(implicit F: Eff[F], hints: Hint): F[Iterable[KeyedMetric]] =
    F.pure(
      inter
        .foldLeft(Map.empty[(String, MetricType), Numbers]) {
          case (acc, (k, v)) =>
            val key = (hints.pathToString(k.name), k.metricType.getOrElse(defaultMetricType))

            acc + (key -> acc.get(key).fold(v)(Numbers.add(v, _)))
        }
        .map { case ((k, t), v) => KeyedMetric(k, t, v) }
        .toList
    )
}

case class KeyedMetric private[keyed] (name: String, metricType: MetricType, value: Numbers)
