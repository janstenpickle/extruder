package extruder.metrics.dropwizard

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.cats.effect.EvalValidation
import extruder.core.{Encode, ExtruderErrors}
import extruder.metrics.MetricEncoder
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoders, DimensionalMetricSettings}
import io.dropwizard.metrics5.{MetricName, MetricRegistry}

import scala.collection.JavaConverters._

trait DropwizardDimensionalEncoders extends DimensionalMetricEncoders {
  override type Sett = DimensionalMetricSettings
  override def defaultSettings: DimensionalMetricSettings = new DimensionalMetricSettings {}
}

trait DropwizardDimensionalEncode extends DropwizardDimensionalEncoders with Encode {
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
          case MetricType.Counter => makeCounter[F](metric)
          case _ => makeGauge[F](metric)
        }
      }
    }
}

trait DropwizardDimensionalRegistryEncoders extends DropwizardDimensionalEncoders {
  override type EncDefault[A] = EvalValidation[A]
  override type EncT[F[_], T] = DropwizardDimensionalMetricsEncoder[F, T]
  override type EncEff[F[_]] = Sync[F]
  override type OutputData = MetricRegistry

  override protected def mkEncoder[F[_], T](
    f: (List[String], Sett, T) => F[Metrics]
  ): DropwizardDimensionalMetricsEncoder[F, T] =
    new DropwizardDimensionalMetricsEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Metrics] = f(path, settings, in)
    }
}

trait DropwizardDimensionalMetricsEncoder[F[_], T] extends MetricEncoder[F, DimensionalMetricSettings, T]

object DropwizardDimensionalMetricsEncoder extends DropwizardDimensionalRegistryEncoders

class DropwizardDimensionalRegistry(
  registry: MetricRegistry = new MetricRegistry(),
  defaultLabels: Map[String, String] = Map.empty,
  namespaceName: Option[String] = None,
  defaultMetricType: MetricType = MetricType.Gauge
) extends DropwizardDimensionalRegistryEncoders
    with DropwizardDimensionalEncode {
  override protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: Sync[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      metric.values.foreach {
        case (tagValues, value) =>
          val tags = metric.labelNames.zip(tagValues).toMap
          val metricName = MetricName.build(metric.name).tagged(tags.asJava)
          val counter = registry.counter(metricName)
          counter.inc(Numbers.toLong(value))
      }
    })

  override protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: Sync[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      metric.values.foreach {
        case (tagValues, value) =>
          val tags = metric.labelNames.zip(tagValues).toMap
          val metricName = MetricName.build(metric.name).tagged(tags.asJava)
          val gauge = registry.gauge(metricName, new SimpleGaugeSupplier).asInstanceOf[SimpleGauge]
          gauge.set(Numbers.toDouble(value))
      }
    })

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: Metrics)(
    implicit F: Sync[F]
  ): F[MetricRegistry] =
    buildMeters(namespaceName, namespace, settings, inter, defaultLabels, defaultMetricType).map(_ => registry)
}
