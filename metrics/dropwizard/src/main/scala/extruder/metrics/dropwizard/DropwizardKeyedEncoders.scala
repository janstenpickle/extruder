package extruder.metrics.dropwizard

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.cats.effect.EvalValidation
import extruder.core.Encode
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.keyed.{KeyedMetric, KeyedMetricEncoders}
import extruder.metrics.{MetricEncoder, MetricSettings}
import io.dropwizard.metrics5.{MetricName, MetricRegistry}

trait DropwizardKeyedEncoders extends KeyedMetricEncoders {
  override type Sett = MetricSettings
  override val defaultSettings: Sett = new MetricSettings {}
}

trait DropwizardKeyedEncode extends DropwizardKeyedEncoders with Encode {
  protected def makeCounter[F[_]](metric: KeyedMetric)(implicit F: EncEff[F]): F[Unit]
  protected def makeGauge[F[_]](metric: KeyedMetric)(implicit F: EncEff[F]): F[Unit]

  protected def buildMeters[F[_]](
    namespace: List[String],
    settings: Sett,
    inter: Metrics,
    defaultMetricType: MetricType
  )(implicit F: EncEff[F]): F[List[Unit]] =
    buildMetrics[F](settings, inter, defaultMetricType).flatMap { metrics =>
      metrics.toList.traverse { metric =>
        metric.metricType match {
          case MetricType.Counter => makeCounter[F](metric)
          case _ => makeGauge[F](metric)
        }
      }
    }
}

trait DropwizardKeyedRegistryEncoders extends DropwizardKeyedEncoders {
  override type EncDefault[A] = EvalValidation[A]
  override type EncT[F[_], T] = DropwizardKeyedMetricsEncoder[F, T]
  override type EncEff[F[_]] = Sync[F]
  override type OutputData = MetricRegistry

  override protected def mkEncoder[F[_], T](
    f: (List[String], Sett, T) => F[Metrics]
  ): DropwizardKeyedMetricsEncoder[F, T] =
    new DropwizardKeyedMetricsEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Metrics] = f(path, settings, in)
    }
}

trait DropwizardKeyedMetricsEncoder[F[_], T] extends MetricEncoder[F, MetricSettings, T]

object DropwizardKeyedMetricsEncoder extends DropwizardKeyedRegistryEncoders

class DropwizardKeyedRegistry(
  registry: MetricRegistry = new MetricRegistry(),
  defaultMetricType: MetricType = MetricType.Gauge
) extends DropwizardKeyedRegistryEncoders
    with DropwizardKeyedEncode {

  override protected def makeCounter[F[_]](metric: KeyedMetric)(implicit F: EncEff[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      val counter = registry.counter(metric.name)
      counter.inc(Numbers.toLong(metric.value))
    })

  override protected def makeGauge[F[_]](metric: KeyedMetric)(implicit F: EncEff[F]): F[Unit] =
    F.suspend(F.catchNonFatal {
      val gauge = registry.gauge(MetricName.build(metric.name), new SimpleGaugeSupplier).asInstanceOf[SimpleGauge]
      gauge.set(Numbers.toDouble(metric.value))
    })

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: Metrics)(
    implicit F: EncEff[F]
  ): F[MetricRegistry] = buildMeters(namespace, settings, inter, defaultMetricType).map(_ => registry)
}
