package extruder.metrics.prometheus

import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import extruder.core.{Encode, ExtruderErrors}
import extruder.metrics.data.{MetricType, Metrics}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoders, DimensionalMetricSettings}
import io.prometheus.client.Collector

trait PrometheusEncoders extends DimensionalMetricEncoders {
  override type Sett = DimensionalMetricSettings
  override def defaultSettings: DimensionalMetricSettings = new DimensionalMetricSettings {}
}

trait PrometheusEncode extends PrometheusEncoders with Encode {
  val Help: String = "Generated"

  protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Collector]
  protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Collector]

  protected def buildCollectors[F[_]: EncEff: ExtruderErrors](
    namespaceName: Option[String],
    namespace: List[String],
    settings: Sett,
    inter: Metrics,
    defaultLabels: Map[String, String],
    defaultMetricType: MetricType
  ): F[List[Collector]] = {
    buildMetrics[F](namespaceName, namespace, settings, inter, defaultLabels, defaultMetricType)
      .flatMap { metrics =>
        metrics.toList.traverse { metric =>
          metric.metricType match {
            case MetricType.Counter => makeCounter[F](metric)
            case _ => makeGauge[F](metric)
          }
        }
      }
  }
}
