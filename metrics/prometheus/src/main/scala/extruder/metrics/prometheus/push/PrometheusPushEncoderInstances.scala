package extruder.metrics.prometheus.push

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.core.ExtruderErrors
import extruder.data.Finalize
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoderInstances}
import io.prometheus.client.{Collector, Counter, Gauge}

trait PrometheusPushEncoderInstances extends DimensionalMetricEncoderInstances {
  protected val Help: String = "Generated"
  protected val InstanceName: String = "instance"

  protected def makeGauge[F[_], S <: PrometheusPushMetricSettings](metric: DimensionalMetric, settings: S)(
    implicit F: Sync[F]
  ): F[Collector] = {
    val gauge = Gauge
      .build(metric.name, Help)
      .labelNames(metric.labelNames :+ InstanceName: _*)
      .create()

    metric.values.foreach { case (l, v) => gauge.labels(l :+ settings.jobInstance: _*).set(Numbers.toDouble(v)) }
    F.pure(gauge)
  }

  protected def makeCounter[F[_], S <: PrometheusPushMetricSettings](metric: DimensionalMetric, settings: S)(
    implicit F: Sync[F]
  ): F[Collector] = {
    val counter = Counter
      .build(metric.name, Help)
      .labelNames(metric.labelNames :+ InstanceName: _*)
      .create()
    metric.values.foreach { case (l, v) => counter.labels(l :+ settings.jobInstance: _*).inc(Numbers.toDouble(v)) }

    F.pure(counter)
  }

  protected def buildCollectors[F[_]: Sync: ExtruderErrors, S <: PrometheusPushMetricSettings](
    settings: S
  )(metrics: Iterable[DimensionalMetric]): F[List[Collector]] =
    metrics.toList.traverse { metric =>
      metric.metricType match {
        case MetricType.Counter => makeCounter[F, S](metric, settings)
        case _ => makeGauge[F, S](metric, settings)
      }
    }

  implicit def prometheusPushFinalize[F[_], S <: PrometheusPushMetricSettings](
    implicit F: Sync[F],
    dimensionalFinalize: Finalize[F, S, Metrics, Iterable[DimensionalMetric]]
  ): Finalize[F, S, Metrics, Unit] =
    new Finalize[F, S, Metrics, Unit] {
      override def run(namespace: List[String], settings: S, inputData: Metrics): F[Unit] =
        dimensionalFinalize.flatMapResult(buildCollectors[F, S](settings)).run(namespace, settings, inputData).flatMap {
          collectors =>
            collectors
              .traverse(
                collector => F.suspend[Unit](F.catchNonFatal(settings.pushGateway.push(collector, settings.jobName)))
              )
              .void
        }
    }
}
