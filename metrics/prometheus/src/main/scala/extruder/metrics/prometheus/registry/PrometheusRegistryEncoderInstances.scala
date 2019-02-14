package extruder.metrics.prometheus.registry

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.core.{ExtruderErrors, Transform}
import extruder.data.PathElement
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoderInstances}
import io.prometheus.client.{Collector, CollectorRegistry, Counter, Gauge}

trait PrometheusRegistryEncoderInstances extends DimensionalMetricEncoderInstances {
  protected val Help = "Generated"

  private def makeKey(metric: DimensionalMetric): String = (metric.labelNames + metric.name).mkString("_").toLowerCase

  protected def makeGauge[F[_]](settings: PrometheusRegistryMetricSettings, metric: DimensionalMetric)(
    implicit F: Sync[F]
  ): F[Collector] = {
    def newGauge: Gauge =
      Gauge
        .build(metric.name, Help)
        .labelNames(metric.labelNames: _*)
        .create()
        .register(settings.registry)

    def setGauge(gauge: Gauge): F[Collector] =
      F.suspend(F.catchNonFatal {
        metric.values.foreach { case (l, v) => gauge.labels(l: _*).set(Numbers.toDouble(v)) }
        gauge
      })

    F.suspend(F.catchNonFatal(settings.gauges.getOrDefault(makeKey(metric), newGauge))).flatMap(setGauge)
  }

  protected def makeCounter[F[_]](settings: PrometheusRegistryMetricSettings, metric: DimensionalMetric)(
    implicit F: Sync[F]
  ): F[Collector] = {
    def newCounter: Counter =
      Counter
        .build(metric.name, Help)
        .labelNames(metric.labelNames: _*)
        .create()
        .register(settings.registry)

    def setCounter(counter: Counter): F[Collector] =
      F.suspend(F.catchNonFatal {
        metric.values.foreach { case (l, v) => counter.labels(l: _*).inc(Numbers.toDouble(v)) }
        counter
      })

    F.suspend(F.catchNonFatal(settings.counters.getOrDefault(makeKey(metric), newCounter))).flatMap(setCounter)
  }

  protected def buildCollectors[F[_]: Sync: ExtruderErrors, S <: PrometheusRegistryMetricSettings](
    namespace: List[String],
    settings: S,
    inter: Metrics
  ): F[List[Collector]] = {
    buildMetrics[F, S](namespace, settings, inter)
      .flatMap { metrics =>
        metrics.toList.traverse { metric =>
          metric.metricType match {
            case MetricType.Counter => makeCounter[F](settings, metric)
            case _ => makeGauge[F](settings, metric)
          }
        }
      }
  }

  implicit def prometheusRegistryTransform[F[_]: ExtruderErrors, S <: PrometheusRegistryMetricSettings](
    implicit F: Sync[F]
  ): Transform[F, S, Metrics, CollectorRegistry] =
    new Transform[F, S, Metrics, CollectorRegistry] {
      override def run(namespace: List[PathElement], settings: S, inputData: Metrics): F[CollectorRegistry] =
        buildCollectors[F, S](settings.pathElementsAsStrings(namespace), settings, inputData)
          .map(_ => settings.registry)
    }
}
