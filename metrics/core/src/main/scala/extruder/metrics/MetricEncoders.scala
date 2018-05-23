package extruder.metrics

import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Monoid, Traverse}
import extruder.core._
import extruder.metrics.data._
import extruder.metrics.syntax.timer._
import shapeless.ops.coproduct.Inject
import shapeless.{Coproduct, Refute}

import scala.concurrent.duration.FiniteDuration

trait MetricEncoders
    extends PrimitiveEncoders
    with StringMapEncoders
    with DerivedEncoders
    with Encoders
    with EncodeTypes {
  override type EncodeData = Metrics

  override protected def monoid: Monoid[EncodeData] = new Monoid[EncodeData] {
    override def empty: EncodeData = Metrics(Map.empty, Map.empty)
    override def combine(x: EncodeData, y: EncodeData): EncodeData = {
      val metrics = y.metrics.foldLeft(x.metrics) {
        case (acc, (k, v)) => acc + (k -> acc.get(k).fold(v)(Numbers.add(_, v)))
      }
      val statuses = x.statuses ++ y.statuses
      Metrics(statuses, metrics)
    }
  }

  override protected def writeValue[F[_]](
    path: List[String],
    value: String
  )(implicit hints: Hint, F: Eff[F]): F[EncodeData] =
    F.pure(Metrics.status(path, value))

  implicit def traversableThrowableEncoder[F[_]: Eff, FF[A] <: TraversableOnce[A]]: Enc[F, FF[Throwable]] = mkEncoder {
    (path, values) =>
      Metrics.single(MetricKey(path, Some(MetricType.Gauge)), Coproduct[Numbers](values.size)).pure[F]
  }

  implicit def traversableEncoder[F[_]: Eff, T, FF[A] <: TraversableOnce[A]](
    implicit encoder: Enc[F, T]
  ): Enc[F, FF[T]] =
    mkEncoder[F, FF[T]] { (path, values) =>
      Traverse[List].traverse(values.toList)(encoder.write(path, _)).map(monoid.combineAll)
    }

  implicit def numericMapEncoder[F[_]: Eff, T](implicit inj: Inject[Numbers, T]): Enc[F, Map[String, T]] =
    mkEncoder[F, Map[String, T]] { (path, values) =>
      Metrics(values.map {
        case (k, v) => MetricKey(path :+ k, None) -> Coproduct[Numbers](v)
      }).pure[F]
    }

  implicit def metricValuesEncoder[F[_]: Eff, T, V[A] <: MetricValue[A]](
    implicit inj: Inject[Numbers, T]
  ): Enc[F, MetricValues[V, T]] =
    mkEncoder[F, MetricValues[V, T]] { (path, mv) =>
      Metrics(mv.values.map {
        case (k, v) => MetricKey(path :+ k, Some(v.metricType)) -> Coproduct[Numbers](v.value)
      }).pure[F]
    }

  implicit def numericEncoder[F[_]: Eff, T](implicit inj: Inject[Numbers, T]): Enc[F, T] = mkEncoder[F, T] {
    (path, value) =>
      Metrics.single(MetricKey(path, None), Coproduct[Numbers](value)).pure[F]
  }

  implicit def metricValueEncoder[F[_]: Eff, T, V[A] <: MetricValue[A]](
    implicit inj: Inject[Numbers, T],
    refute: Refute[Enc[F, V[T]]]
  ): Enc[F, V[T]] = mkEncoder[F, V[T]] { (path, mv) =>
    Metrics.single(MetricKey(path, Some(mv.metricType)), Coproduct[Numbers](mv.value)).pure[F]
  }

  implicit def timerEncoder[F[_]: Eff]: Enc[F, TimerValue[Long]] = mkEncoder[F, TimerValue[Long]] { (path, tv) =>
    val v = if (tv.isFinished) tv else tv.checkpoint()
    Metrics.single(MetricKey(path, Some(MetricType.Timer)), Coproduct[Numbers](v.value)).pure[F]
  }

  implicit def durationEncoder[F[_]: Eff]: Enc[F, FiniteDuration] = mkEncoder { (path, dur) =>
    Metrics.single(MetricKey(path, Some(MetricType.Timer)), Coproduct[Numbers](dur.toMillis)).pure[F]
  }

}

trait MetricEncoder[F[_], T] extends Encoder[F, T, Metrics]

trait MetricsHints extends Hints {
  override def pathToString(path: List[String]): String = path.map(snakeCaseTransformation).mkString(".")
}
