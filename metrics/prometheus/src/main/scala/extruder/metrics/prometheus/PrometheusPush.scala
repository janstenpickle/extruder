package extruder.metrics.prometheus

import cats.Traverse
import cats.effect.Async
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import extruder.metrics.MetricEncoder
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricSettings}
import io.prometheus.client.exporter.PushGateway
import io.prometheus.client.{Collector, Counter, Gauge}

trait PushEncoders extends PrometheusEncoders {
  override type EncT[F[_], T] = PushMetricsEncoder[F, T]
  override type EncEff[F[_]] = Async[F]

  override protected def mkEncoder[F[_], T](f: (List[String], Sett, T) => F[Metrics]): PushMetricsEncoder[F, T] =
    new PushMetricsEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Metrics] = f(path, settings, in)
    }
}

trait PushMetricsEncoder[F[_], T] extends MetricEncoder[F, DimensionalMetricSettings, T]

object PushMetricsEncoder extends PushEncoders

case class PrometheusPush(
  gateway: PushGateway,
  jobName: String,
  jobInstance: String,
  defaultLabels: Map[String, String] = Map.empty,
  namespaceName: Option[String] = None,
  defaultMetricType: MetricType = MetricType.Gauge
) extends PushEncoders
    with PrometheusEncode {
  override type OutputData = Unit

  override protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Collector] = {
    val counter = Counter
      .build(metric.name, Help)
      .labelNames(metric.labelNames.toSeq: _*)
      .create()
    metric.values.foreach { case (l, v) => counter.labels(l: _*).inc(Numbers.toDouble(v)) }
    F.pure(counter)
  }

  override protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Collector] = {
    val gauge = Gauge
      .build(metric.name, Help)
      .labelNames(metric.labelNames.toSeq: _*)
      .create()
    metric.values.foreach { case (l, v) => gauge.labels(l: _*).set(Numbers.toDouble(v)) }
    F.pure(gauge)
  }

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: EncodeData)(
    implicit F: Async[F]
  ): F[Unit] =
    for {
      collectors <- buildCollectors(
        namespaceName,
        namespace,
        settings,
        inter,
        defaultLabels + ("instance" -> jobInstance),
        defaultMetricType
      )
      _ <- Traverse[List].traverse(collectors)(
        collector => F.async[Unit](cb => cb(Either.catchNonFatal(gateway.push(collector, jobName))))
      )
    } yield ()

}
