package extruder.metrics.spectator

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.netflix.spectator.api._
import extruder.cats.effect.EvalValidation
import extruder.core.{Encode, ExtruderErrors}
import extruder.metrics._
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoders, DimensionalMetricSettings}

import scala.collection.JavaConverters._

trait SpectatorEncoders extends DimensionalMetricEncoders {
  override type Sett = DimensionalMetricSettings
  override def defaultSettings: DimensionalMetricSettings = new DimensionalMetricSettings {}
}

trait SpectatorEncode extends SpectatorEncoders with Encode {
  protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Unit]
  protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Unit]

  protected def buildMeters[F[_]: EncEff: ExtruderErrors](
    namespaceName: Option[String],
    namespace: List[String],
    settings: Sett,
    inter: Metrics,
    defaultLabels: Map[String, String],
    defaultMetricType: MetricType
  ): F[List[Unit]] =
    buildMetrics[F](namespaceName, namespace, settings, inter, defaultLabels, defaultMetricType).flatMap { metrics =>
      metrics.toList.traverse { metric =>
        metric.metricType match {
          case MetricType.Counter => makeCounter(metric)
          case _ => makeGauge(metric)
        }
      }
    }
}

trait RegistryEncoders extends SpectatorEncoders {
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

class SpectatorRegistry(
  registry: Registry,
  defaultLabels: Map[String, String] = Map.empty,
  namespaceName: Option[String] = None,
  defaultMetricType: MetricType = MetricType.Gauge
) extends RegistryEncoders
    with SpectatorEncode {
  override type OutputData = Registry

  override protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Unit] = {
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

  override protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: EncEff[F]): F[Unit] = {
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

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: EncodeData)(
    implicit F: EncEff[F]
  ): F[Registry] =
    buildMeters(namespaceName, namespace, settings, inter, defaultLabels, defaultMetricType).map(_ => registry)

}
