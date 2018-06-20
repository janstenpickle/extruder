package extruder.metrics.data

import cats.syntax.applicative._
import cats.syntax.flatMap._
import extruder.core.{MapEncoder, Settings, Show}
import extruder.effect.ExtruderMonadError

sealed trait MetricKey {
  def name: String
  def path: List[String]
  def metricType: Option[MetricType]
}

case class SimpleMetricKey(name: String, path: List[String], metricType: Option[MetricType]) extends MetricKey

case class DimensionalMetricKey(
  name: String,
  path: List[String],
  dimensions: Map[String, String],
  metricType: Option[MetricType]
) extends MetricKey

object MetricKey {
  val ErrorMsg = "Cannot create metric with an empty name and dimension name"

  def apply[F[_]](path: List[String], metricType: Option[MetricType])(implicit F: ExtruderMonadError[F]): F[MetricKey] =
    path.reverse match {
      case dimensionValue :: dimensionName :: name :: _ =>
        DimensionalMetricKey(name, path.dropRight(3), Map(dimensionName -> dimensionValue), metricType)
          .asInstanceOf[MetricKey]
          .pure
      case name :: _ => SimpleMetricKey(name, path.dropRight(1), metricType).asInstanceOf[MetricKey].pure
      case _ => F.validationFailure("Cannot create a metric key with an empty name")
    }

  def apply[F[_]](path: List[String], metricType: MetricType)(implicit F: ExtruderMonadError[F]): F[MetricKey] =
    apply[F](path, Some(metricType))

  def apply[F[_]](path: List[String], dimensionValue: String, metricType: Option[MetricType])(
    implicit F: ExtruderMonadError[F]
  ): F[MetricKey] = path.reverse match {
    case dimensionName :: name :: _ =>
      DimensionalMetricKey(name, path.dropRight(2), Map(dimensionName -> dimensionValue), metricType)
        .asInstanceOf[MetricKey]
        .pure[F]
    case Nil => SimpleMetricKey(dimensionValue, path, metricType).asInstanceOf[MetricKey].pure[F]
    case _ => F.validationFailure(ErrorMsg)
  }

  def apply[F[_], A](path: List[String], dimensionValue: A, metricType: Option[MetricType])(
    implicit F: ExtruderMonadError[F],
    show: Show[A]
  ): F[MetricKey] = apply[F](path, show.show(dimensionValue), metricType)

  def apply[F[_]](path: List[String], dimensions: Map[String, String], metricType: Option[MetricType])(
    implicit F: ExtruderMonadError[F]
  ): F[MetricKey] = path.reverse match {
    case name :: _ => DimensionalMetricKey(name, path.dropRight(1), dimensions, metricType).asInstanceOf[MetricKey].pure
    case _ => F.validationFailure(ErrorMsg)
  }

  def apply[F[_], K, V](
    path: List[String],
    dimensions: Map[K, V],
    metricType: Option[MetricType]
  )(implicit F: ExtruderMonadError[F], showK: Show[K], showV: Show[V]): F[MetricKey] =
    apply[F](path, dimensions.map { case (k, v) => showK.show(k) -> showV.show(v) }, metricType)

  def apply[F[_], A <: Product with Serializable](
    path: List[String],
    settings: Settings,
    dimensions: A,
    metricType: Option[MetricType]
  )(implicit F: ExtruderMonadError[F], enc: MapEncoder[F, A]): F[MetricKey] =
    enc.write(List.empty, settings, dimensions).flatMap(apply[F](path, _, metricType))

  def apply[F[_]](path: List[String], dimension: (String, String), metricType: Option[MetricType])(
    implicit F: ExtruderMonadError[F]
  ): F[MetricKey] = path.reverse match {
    case name :: _ =>
      DimensionalMetricKey(name, path.dropRight(1), Map(dimension), metricType).asInstanceOf[MetricKey].pure
    case _ => F.validationFailure(ErrorMsg)
  }

  def apply[F[_], K, V](path: List[String], dimension: (K, V), metricType: Option[MetricType])(
    implicit F: ExtruderMonadError[F],
    showK: Show[K],
    showV: Show[V]
  ): F[MetricKey] = apply[F](path, showK.show(dimension._1) -> showV.show(dimension._2), metricType)
}
