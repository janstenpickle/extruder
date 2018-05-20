package extruder.metrics.dropwizard

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.core.Encode
import extruder.effect.ExtruderAsync
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import extruder.metrics.dimensional.{DimensionalMetric, DimensionalMetricEncoders}
import extruder.metrics.{snakeCaseTransformation, MetricEncoder}
import io.dropwizard.metrics5.{MetricName, MetricRegistry}

import scala.collection.JavaConverters._

trait DropwizardDimensionalEncoders extends DimensionalMetricEncoders {
  override type Hint = DropwizardHints
  override def labelTransform(value: String): String = snakeCaseTransformation(value)
}

trait DropwizardDimensionalEncode extends DropwizardDimensionalEncoders with Encode {
  protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: Eff[F]): F[Unit]
  protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: Eff[F]): F[Unit]

  protected def buildMeters[F[_]](
    namespaceName: Option[String],
    namespace: List[String],
    inter: Metrics,
    defaultLabels: Map[String, String],
    defaultMetricType: MetricType
  )(implicit F: Eff[F], hints: Hint): F[List[Unit]] =
    buildMetrics[F](namespaceName, namespace, inter, defaultLabels, defaultMetricType).flatMap { metrics =>
      metrics.toList.traverse { metric =>
        metric.metricType match {
          case MetricType.Counter => makeCounter[F](metric)
          case _ => makeGauge[F](metric)
        }
      }
    }
}

trait DropwizardDimensionalRegistryEncoders extends DropwizardDimensionalEncoders {
  override type Enc[F[_], T] = DropwizardDimensionalMetricsEncoder[F, T]
  override type Eff[F[_]] = ExtruderAsync[F]
  override type OutputData = MetricRegistry

  override protected def mkEncoder[F[_], T](
    f: (List[String], T) => F[Metrics]
  ): DropwizardDimensionalMetricsEncoder[F, T] =
    new DropwizardDimensionalMetricsEncoder[F, T] {
      override def write(path: List[String], in: T): F[Metrics] = f(path, in)
    }
}

trait DropwizardDimensionalMetricsEncoder[F[_], T] extends MetricEncoder[F, T]

object DropwizardDimensionalMetricsEncoder extends DropwizardDimensionalRegistryEncoders

class DropwizardDimensionalRegistry(
  registry: MetricRegistry = new MetricRegistry(),
  defaultLabels: Map[String, String] = Map.empty,
  namespaceName: Option[String] = None,
  defaultMetricType: MetricType = MetricType.Gauge
) extends DropwizardDimensionalRegistryEncoders
    with DropwizardDimensionalEncode {
  override protected def makeCounter[F[_]](metric: DimensionalMetric)(implicit F: ExtruderAsync[F]): F[Unit] =
    F.async(
      cb =>
        cb(Either.catchNonFatal {
          metric.values.foreach {
            case (tagValues, value) =>
              val tags = metric.labelNames.zip(tagValues).toMap
              val metricName = MetricName.build(metric.name).tagged(tags.asJava)
              val counter = registry.counter(metricName)
              counter.inc(Numbers.toLong(value))
          }
        })
    )

  override protected def makeGauge[F[_]](metric: DimensionalMetric)(implicit F: ExtruderAsync[F]): F[Unit] =
    F.async(
      cb =>
        cb(Either.catchNonFatal {
          metric.values.foreach {
            case (tagValues, value) =>
              val tags = metric.labelNames.zip(tagValues).toMap
              val metricName = MetricName.build(metric.name).tagged(tags.asJava)
              val gauge = registry.gauge(metricName, new SimpleGaugeSupplier).asInstanceOf[SimpleGauge]
              gauge.set(Numbers.toDouble(value))
          }
        })
    )

  override protected def finalizeOutput[F[_]](
    namespace: List[String],
    inter: Metrics
  )(implicit F: ExtruderAsync[F], hints: DropwizardHints): F[MetricRegistry] =
    buildMeters(namespaceName, namespace, inter, defaultLabels, defaultMetricType).map(_ => registry)
}
