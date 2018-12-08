package extruder.metrics.dropwizard.dimensional

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.data.Finalize
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoderInstances}
import extruder.metrics.dropwizard.{SimpleGauge, SimpleGaugeSupplier}
import io.dropwizard.metrics5.{MetricName, MetricRegistry}

import scala.collection.JavaConverters._

trait DropwizardDimensionalEncoderInstances extends DimensionalMetricEncoderInstances {
  protected def makeCounter[F[_]](registry: MetricRegistry, metric: DimensionalMetric)(implicit F: Sync[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      metric.values.foreach {
        case (tagValues, value) =>
          val tags = metric.labelNames.zip(tagValues).toMap
          val metricName = MetricName.build(metric.name).tagged(tags.asJava)
          val counter = registry.counter(metricName)
          counter.inc(Numbers.toLong(value))
      }
    })

  protected def makeGauge[F[_]](registry: MetricRegistry, metric: DimensionalMetric)(implicit F: Sync[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      metric.values.foreach {
        case (tagValues, value) =>
          val tags = metric.labelNames.zip(tagValues).toMap
          val metricName = MetricName.build(metric.name).tagged(tags.asJava)
          val gauge = registry.gauge(metricName, new SimpleGaugeSupplier).asInstanceOf[SimpleGauge]
          gauge.set(Numbers.toDouble(value))
      }
    })

  implicit def dropwizardDimensionalFinalize[F[_], S <: DropwizardDimensionalMetricSettings](
    implicit F: Sync[F],
    dimensionalFinalize: Finalize[F, S, Metrics, Iterable[DimensionalMetric]]
  ): Finalize[F, S, Metrics, MetricRegistry] =
    new Finalize[F, S, Metrics, MetricRegistry] {
      override def run(namespace: List[String], settings: S, inputData: Metrics): F[MetricRegistry] =
        dimensionalFinalize.run(namespace, settings, inputData).flatMap { metrics =>
          metrics.toList
            .traverse { metric =>
              metric.metricType match {
                case MetricType.Counter => makeCounter[F](settings.registry, metric)
                case _ => makeGauge[F](settings.registry, metric)
              }
            }
            .map(_ => settings.registry)
        }
    }
}
