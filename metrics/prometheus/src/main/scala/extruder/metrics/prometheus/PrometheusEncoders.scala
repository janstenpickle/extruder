package extruder.metrics.prometheus

import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import extruder.core.{Encode, HintsCompanion}
import extruder.metrics._
import extruder.metrics.data.{MetricType, Metrics}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoders}
import io.prometheus.client.Collector

trait PrometheusEncoders extends DimensionalMetricEncoders {
  override type Hint = PrometheusHints
  override def labelTransform(value: String): String = snakeCaseTransformation(value)
}

trait PrometheusEncode extends PrometheusEncoders with Encode {
  val Help: String = "Generated"

  protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: Eff[F]): F[Collector]
  protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: Eff[F]): F[Collector]

  protected def buildCollectors[F[_]](
    namespaceName: Option[String],
    namespace: List[String],
    inter: Metrics,
    defaultLabels: Map[String, String],
    defaultMetricType: MetricType
  )(implicit F: Eff[F], hints: Hint): F[List[Collector]] = {
    buildMetrics[F](namespaceName, namespace, inter, defaultLabels, defaultMetricType)
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

trait PrometheusHints extends MetricsHints

object PrometheusHints extends HintsCompanion[PrometheusHints] {
  override implicit def default: PrometheusHints = new PrometheusHints {}
}
