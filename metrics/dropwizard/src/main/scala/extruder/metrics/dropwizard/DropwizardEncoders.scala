package extruder.metrics.dropwizard

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.codahale.metrics.MetricRegistry.MetricSupplier
import com.codahale.metrics.{Gauge, MetricRegistry}
import extruder.core.{Encode, HintsCompanion}
import extruder.effect.ExtruderAsync
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.keyed.{KeyedMetric, KeyedMetricEncoders}
import extruder.metrics.{MetricEncoder, MetricsHints}

trait DropwizardEncoders extends KeyedMetricEncoders {
  override type Hint = DropwizardHints
}

trait DropwizardEncode extends DropwizardEncoders with Encode {
  protected def makeCounter[F[_]](metric: KeyedMetric)(implicit F: Eff[F]): F[Unit]
  protected def makeGauge[F[_]](metric: KeyedMetric)(implicit F: Eff[F]): F[Unit]

  protected def buildMeters[F[_]](namespace: List[String], inter: Metrics, defaultMetricType: MetricType)(
    implicit F: Eff[F],
    hints: Hint
  ): F[List[Unit]] =
    buildMetrics[F](inter, defaultMetricType).flatMap { metrics =>
      metrics.toList.traverse { metric =>
        metric.metricType match {
          case MetricType.Counter => makeCounter[F](metric)
          case _ => makeGauge[F](metric)
        }
      }
    }
}

trait MetricRegistryEncoders extends DropwizardEncoders {
  override type Enc[F[_], T] = DropwizardMetricsEncoder[F, T]
  override type Eff[F[_]] = ExtruderAsync[F]
  override type OutputData = MetricRegistry

  override protected def mkEncoder[F[_], T](f: (List[String], T) => F[Metrics]): DropwizardMetricsEncoder[F, T] =
    new DropwizardMetricsEncoder[F, T] {
      override def write(path: List[String], in: T): F[Metrics] = f(path, in)
    }
}

trait DropwizardMetricsEncoder[F[_], T] extends MetricEncoder[F, T]

object DropwizardMetricsEncoder extends MetricRegistryEncoders

class DropwizardRegistry(
  registry: MetricRegistry = new MetricRegistry(),
  defaultMetricType: MetricType = MetricType.Gauge
) extends MetricRegistryEncoders
    with DropwizardEncode {

  override protected def makeCounter[F[_]](metric: KeyedMetric)(implicit F: ExtruderAsync[F]): F[Unit] =
    F.async(
      cb =>
        cb(Either.catchNonFatal {
          val counter = registry.counter(metric.name)
          counter.inc(Numbers.toLong(metric.value))
        })
    )

  override protected def makeGauge[F[_]](metric: KeyedMetric)(implicit F: ExtruderAsync[F]): F[Unit] =
    F.async(
      cb =>
        cb(Either.catchNonFatal {
          val gauge = registry.gauge(metric.name, new SimpleGaugeSupplier).asInstanceOf[SimpleGauge]
          gauge.set(Numbers.toDouble(metric.value))
        })
    )

  override protected def finalizeOutput[F[_]](namespace: List[String], inter: Metrics)(
    implicit F: Eff[F],
    hints: DropwizardHints
  ): F[MetricRegistry] = buildMeters(namespace, inter, defaultMetricType).map(_ => registry)
}

trait DropwizardHints extends MetricsHints

object DropwizardHints extends HintsCompanion[DropwizardHints] {
  override implicit def default: DropwizardHints = new DropwizardHints {}
}

case class SimpleGauge(initial: Double) extends Gauge[Double] {
  private var _value = initial
  def set(value: Double): Unit = _value = value
  override def getValue: Double = _value
}

class SimpleGaugeSupplier extends MetricSupplier[Gauge[_]] {
  override def newMetric(): SimpleGauge = SimpleGauge(0.0)
}
