package extruder.metrics.keyed

import extruder.metrics.MetricEncoders
import extruder.metrics.data._

trait KeyedMetricEncoders extends MetricEncoders {
  protected def buildMetrics[F[_]](settings: Sett, inter: Metrics, defaultMetricType: MetricType)(
    implicit F: EncEff[F]
  ): F[Iterable[KeyedMetric]] =
    F.pure(
      inter
        .foldLeft(Map.empty[(String, MetricType), Numbers]) {
          case (acc, (k, v)) =>
            val keys = k match {
              case key: SimpleMetricKey =>
                Map(settings.pathToString(key.path :+ key.name) -> key.metricType.getOrElse(defaultMetricType))
              case key: DimensionalMetricKey =>
                val metricType = key.metricType.getOrElse(defaultMetricType)
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
}

case class KeyedMetric private[keyed] (name: String, metricType: MetricType, value: Numbers)
