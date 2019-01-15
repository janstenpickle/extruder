package extruder.metrics.spectator

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.netflix.spectator.api.Registry
import extruder.core.Transform
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoderInstances}

import scala.collection.JavaConverters._

trait SpectatorEncoderInstances extends DimensionalMetricEncoderInstances {

  protected def makeCounter[F[_]](registry: Registry, metric: DimensionalMetric)(implicit F: Sync[F]): F[Unit] = {
    def incrementCounters: F[Unit] = F.catchNonFatal {
      metric.values.foreach {
        case (labels, value) =>
          val id = registry.createId(metric.name).withTags(metric.labelNames.zip(labels).toMap.asJava)
          val counter = registry.counter(id)
          counter.increment(Numbers.toLong(value))
      }
    }

    F.suspend(incrementCounters)
  }

  protected def makeGauge[F[_]](registry: Registry, metric: DimensionalMetric)(implicit F: Sync[F]): F[Unit] = {
    def setGauges: F[Unit] = F.catchNonFatal {
      metric.values.foreach {
        case (labels, value) =>
          val id = registry.createId(metric.name).withTags(metric.labelNames.zip(labels).toMap.asJava)
          val gauge = registry.gauge(id)
          gauge.set(Numbers.toDouble(value))
      }
    }

    F.suspend(setGauges)
  }

  implicit def spectatorTransform[F[_], S <: SpectatorMetricSettings](
    implicit F: Sync[F],
    dimensionalFinalize: Transform[F, S, Metrics, Iterable[DimensionalMetric]]
  ): Transform[F, S, Metrics, Registry] =
    new Transform[F, S, Metrics, Registry] {
      override def run(namespace: List[String], settings: S, inputData: Metrics): F[Registry] =
        dimensionalFinalize.run(namespace, settings, inputData).flatMap { metrics =>
          metrics.toList
            .traverse { metric =>
              metric.metricType match {
                case MetricType.Counter => makeCounter(settings.registry, metric)
                case _ => makeGauge(settings.registry, metric)
              }
            }
            .map(_ => settings.registry)
        }
    }
}
