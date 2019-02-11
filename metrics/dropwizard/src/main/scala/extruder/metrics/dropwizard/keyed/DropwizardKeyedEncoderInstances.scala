package extruder.metrics.dropwizard.keyed

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.core.Transform
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dropwizard.{SimpleGauge, SimpleGaugeSupplier}
import extruder.metrics.keyed.{KeyedMetric, KeyedMetricEncoderInstances}
import io.dropwizard.metrics5.{MetricName, MetricRegistry}

trait DropwizardKeyedEncoderInstances extends KeyedMetricEncoderInstances {

  protected def makeCounter[F[_]](registry: MetricRegistry, metric: KeyedMetric)(implicit F: Sync[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      val counter = registry.counter(metric.name)
      counter.inc(Numbers.toLong(metric.value))
    })

  protected def makeGauge[F[_]](registry: MetricRegistry, metric: KeyedMetric)(implicit F: Sync[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      val gauge = registry.gauge(MetricName.build(metric.name), new SimpleGaugeSupplier).asInstanceOf[SimpleGauge]
      gauge.set(Numbers.toDouble(metric.value))
    })

  implicit def dropwizardKeyedTransform[F[_], S <: DropwizardKeyedMetricSettings](
    implicit F: Sync[F],
    keyedTransform: Transform[F, S, Metrics, Iterable[KeyedMetric]]
  ): Transform[F, S, Metrics, MetricRegistry] =
    new Transform[F, S, Metrics, MetricRegistry] {
      override def run(namespace: List[String], settings: S, inputData: Metrics): F[MetricRegistry] =
        keyedTransform.run(namespace, settings, inputData).flatMap { metrics =>
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
