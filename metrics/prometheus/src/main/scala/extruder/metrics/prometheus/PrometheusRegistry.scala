package extruder.metrics.prometheus

import cats.effect.Sync
import cats.syntax.functor._
import extruder.cats.effect.EvalValidation
import extruder.metrics.MetricEncoder
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricSettings}
import io.prometheus.client.{Collector, CollectorRegistry, Counter, Gauge}

import scala.collection.concurrent.TrieMap

trait RegistryEncoders extends PrometheusEncoders {
  override type EncDefault[A] = EvalValidation[A]
  override type EncT[F[_], T] = RegistryMetricsEncoder[F, T]
  override type EncEff[F[_]] = Sync[F]

  override protected def mkEncoder[F[_], T](f: (List[String], Sett, T) => F[Metrics]): RegistryMetricsEncoder[F, T] =
    new RegistryMetricsEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Metrics] = f(path, settings, in)
    }
}
trait RegistryMetricsEncoder[F[_], T] extends MetricEncoder[F, DimensionalMetricSettings, T]

object RegistryMetricsEncoder extends RegistryEncoders

class PrometheusRegistry(
  registry: CollectorRegistry = new CollectorRegistry(),
  defaultLabels: Map[String, String] = Map.empty,
  namespaceName: Option[String] = None,
  defaultMetricType: MetricType = MetricType.Gauge
) extends RegistryEncoders
    with PrometheusEncode {
  override type OutputData = CollectorRegistry

  private val counters: TrieMap[String, Counter] = new TrieMap[String, Counter]()
  private val gauges: TrieMap[String, Gauge] = new TrieMap[String, Gauge]()

  private def makeKey(metric: DimensionalMetric): String = (metric.labelNames + metric.name).mkString("_").toLowerCase

  override protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Collector] = {
    def newCounter: Counter =
      Counter
        .build(metric.name, Help)
        .labelNames(metric.labelNames.toSeq: _*)
        .create()
        .register(registry)

    def incrementCounter: F[Collector] = F.catchNonFatal {
      val counter = counters.getOrElseUpdate(makeKey(metric), newCounter)

      metric.values.foreach { case (l, v) => counter.labels(l: _*).inc(Numbers.toDouble(v)) }
      counter
    }

    F.suspend(incrementCounter)
  }

  override protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Collector] = {
    def newGauge: Gauge =
      Gauge
        .build(metric.name, Help)
        .labelNames(metric.labelNames: _*)
        .create()
        .register(registry)

    def setGauge: F[Collector] = F.catchNonFatal {
      val gauge = gauges.getOrElseUpdate(makeKey(metric), newGauge)

      metric.values.foreach { case (l, v) => gauge.labels(l: _*).set(Numbers.toDouble(v)) }
      gauge
    }

    F.suspend(setGauge)
  }

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: EncodeData)(
    implicit F: EncEff[F]
  ): F[CollectorRegistry] =
    buildCollectors(namespaceName, namespace, settings, inter, defaultLabels, defaultMetricType).map(_ => registry)
}
