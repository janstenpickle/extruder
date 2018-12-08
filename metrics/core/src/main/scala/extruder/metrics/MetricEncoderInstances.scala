package extruder.metrics

import cats.instances.list._
import cats.syntax.functor._
import cats.{Applicative, Monad, Monoid, Traverse}
import extruder.core._
import extruder.data.StringWriter
import extruder.map.MapEncoderInstances
import extruder.metrics.data._
import extruder.metrics.syntax.timer._
import shapeless.ops.coproduct.Inject
import shapeless.{Coproduct, LowPriority, Refute}

import scala.concurrent.duration.FiniteDuration

trait MetricEncoderInstances extends MapEncoderInstances {
  implicit val metricsMonoid: Monoid[Metrics] = new Monoid[Metrics] {
    override def empty: Metrics = Map.empty
    override def combine(x: Metrics, y: Metrics): Metrics =
      y.foldLeft(x) {
        case (acc, (k, v)) => acc + (k -> acc.get(k).fold(v)(Numbers.add(_, v)))
      }
  }

  implicit def metricsStringWriter[F[_], S](implicit F: Applicative[F]): StringWriter[F, S, Metrics] =
    new StringWriter[F, S, Metrics] {
      override def write(path: List[String], settings: S, value: String): F[Metrics] =
        F.pure(Metrics.status(path, value))
    }

  implicit def traversableThrowableEncoder[F[_]: Applicative: ExtruderErrors, FF[A] <: TraversableOnce[A], S]: EncoderT[
    F,
    S,
    FF[Throwable],
    Metrics
  ] = EncoderT.make[F, S, FF[Throwable], Metrics] { (path, _, values) =>
    MetricKey[F](path, MetricType.Gauge).map(key => Metrics.single(key, Coproduct[Numbers](values.size)))
  }

  implicit def traversableEncoder[F[_]: Applicative, T, FF[A] <: TraversableOnce[A], S](
    implicit encoder: EncoderT[F, S, T, Metrics],
    monoid: Monoid[Metrics]
  ): EncoderT[F, S, FF[T], Metrics] =
    EncoderT.make[F, S, FF[T], data.Metrics] { (path, settings, values) =>
      Traverse[List].traverse(values.toList)(encoder.write(path, settings, _)).map(monoid.combineAll)
    }

  implicit def numericMapEncoder[F[_]: Applicative: ExtruderErrors, K: Show, V, S](
    implicit inj: Inject[Numbers, V]
  ): EncoderT[F, S, Map[K, V], Metrics] =
    EncoderT.make[F, S, Map[K, V], Metrics] { (path, _, values) =>
      makeKeys[F, K, V](path, values, (k, _) => MetricKey[F, K](path, k, None))
    }

  implicit def numericMapEncoderObjectKey[F[_]: Monad: ExtruderErrors, K <: Product with Serializable, T, S <: MetricSettings](
    implicit inj: Inject[Numbers, T],
    enc: EncoderT[F, Settings, K, Map[String, String]],
    lp: LowPriority
  ): EncoderT[F, S, Map[K, T], Metrics] =
    EncoderT.make[F, S, Map[K, T], Metrics] { (path, settings, values) =>
      makeKeys[F, K, T](path, values, (k, _) => MetricKey[F, K](path, settings.mapEncoderSettings, k, None))
    }

  implicit def numericMapEncoderMapKey[F[_]: Applicative: ExtruderErrors, KK: Show, KV: Show, T, S](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, Map[Map[KK, KV], T], Metrics] =
    EncoderT.make[F, S, Map[Map[KK, KV], T], Metrics] { (path, _, values) =>
      makeKeys[F, Map[KK, KV], T](path, values, (dims, _) => MetricKey[F, KK, KV](path, dims, None))
    }

  implicit def numericMapEncoderTupleKey[F[_]: Applicative: ExtruderErrors, KK: Show, KV: Show, T, S](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, Map[(KK, KV), T], Metrics] =
    EncoderT.make[F, S, Map[(KK, KV), T], Metrics] { (path, _, values) =>
      makeKeys[F, (KK, KV), T](path, values, (dim, _) => MetricKey[F, KK, KV](path, dim, None))
    }

  implicit def metricValuesEncoder[F[_]: Applicative: ExtruderErrors, K: Show, T, V[A] <: MetricValue[A], S](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, MetricValues[V, K, T], Metrics] =
    EncoderT.make[F, S, MetricValues[V, K, T], Metrics] { (path, _, mv) =>
      makeKeys[F, K, V[T], T](path, mv.values, (k, v) => MetricKey[F, K](path, k, Some(v.metricType)), _.value)
    }

  protected def makeKeys[F[_]: Applicative, K, V](
    path: List[String],
    values: Map[K, V],
    makeKey: (K, V) => F[MetricKey]
  )(implicit inj: Inject[Numbers, V]): F[Metrics] = makeKeys[F, K, V, V](path, values, makeKey, identity)

  protected def makeKeys[F[_]: Applicative, K, V, T](
    path: List[String],
    values: Map[K, V],
    makeKey: (K, V) => F[MetricKey],
    makeValue: V => T
  )(implicit inj: Inject[Numbers, T], monoid: Monoid[Metrics]): F[Metrics] =
    Traverse[List]
      .traverse(values.toList) {
        case (k, v) =>
          makeKey(k, v).map { key =>
            Metrics.single(key, Coproduct[Numbers](makeValue(v)))
          }
      }
      .map(monoid.combineAll(_))

  implicit def metricValuesObjectKey[F[_]: Monad: ExtruderErrors, K <: Product with Serializable, T, V[A] <: MetricValue[
    A
  ], S <: MetricSettings](
    implicit inj: Inject[Numbers, T],
    enc: EncoderT[F, Settings, K, Map[String, String]],
    lp: LowPriority
  ): EncoderT[F, S, MetricValues[V, K, T], Metrics] =
    EncoderT.make[F, S, MetricValues[V, K, T], Metrics] { (path, settings, mv) =>
      makeKeys[F, K, V[T], T](
        path,
        mv.values,
        (k, v) => MetricKey[F, K](path, settings.mapEncoderSettings, k, Some(v.metricType)),
        _.value
      )
    }

  implicit def metricValuesMapKey[F[_]: Applicative: ExtruderErrors, KK, KV, T, V[A] <: MetricValue[A], S](
    implicit inj: Inject[Numbers, T],
    showK: Show[KK],
    showKK: Show[KV]
  ): EncoderT[F, S, MetricValues[V, Map[KK, KV], T], Metrics] =
    EncoderT.make[F, S, MetricValues[V, Map[KK, KV], T], Metrics] { (path, _, mv) =>
      makeKeys[F, Map[KK, KV], V[T], T](
        path,
        mv.values,
        (dims, v) => MetricKey[F, KK, KV](path, dims, Some(v.metricType)),
        _.value
      )
    }

  implicit def metricValuesTupleKey[F[_]: Applicative: ExtruderErrors, KK, KV, T, V[A] <: MetricValue[A], S](
    implicit inj: Inject[Numbers, T],
    showKK: Show[KK],
    showKV: Show[KV]
  ): EncoderT[F, S, MetricValues[V, (KK, KV), T], Metrics] =
    EncoderT.make[F, S, MetricValues[V, (KK, KV), T], Metrics] { (path, _, mv) =>
      makeKeys[F, (KK, KV), V[T], T](path, mv.values, {
        case (k, v) => MetricKey[F, KK, KV](path, k, Some(v.metricType))
      }, _.value)
    }

  implicit def numericEncoder[F[_]: Applicative: ExtruderErrors, T, S](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, T, Metrics] =
    EncoderT.make[F, S, T, Metrics] { (path, _, value) =>
      MetricKey[F](path, None).map(key => Metrics.single(key, Coproduct[Numbers](value)))
    }

  implicit def metricValueEncoder[F[_]: Applicative: ExtruderErrors, T, V[A] <: MetricValue[A], S](
    implicit inj: Inject[Numbers, T],
    refute: Refute[EncoderT[F, S, V[T], Metrics]]
  ): EncoderT[F, S, V[T], Metrics] = EncoderT.make[F, S, V[T], Metrics] { (path, _, mv) =>
    MetricKey[F](path, Some(mv.metricType)).map(key => Metrics.single(key, Coproduct[Numbers](mv.value)))
  }

  implicit def timerEncoder[F[_]: Applicative: ExtruderErrors, S]: EncoderT[F, S, TimerValue[Long], Metrics] =
    EncoderT.make[F, S, TimerValue[Long], Metrics] { (path, _, tv) =>
      val v = if (tv.isFinished) tv else tv.checkpoint()
      MetricKey[F](path, Some(MetricType.Timer)).map(key => Metrics.single(key, Coproduct[Numbers](v.value)))
    }

  implicit def durationEncoder[F[_]: Applicative: ExtruderErrors, S]: EncoderT[F, S, FiniteDuration, Metrics] =
    EncoderT.make[F, S, FiniteDuration, Metrics] { (path, _, dur) =>
      MetricKey[F](path, Some(MetricType.Timer)).map(key => Metrics.single(key, Coproduct[Numbers](dur.toMillis)))
    }

}
