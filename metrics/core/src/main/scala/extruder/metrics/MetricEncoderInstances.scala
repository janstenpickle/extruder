package extruder.metrics

import cats.instances.list._
import cats.syntax.functor._
import cats.{Applicative, Monad, Monoid, Traverse}
import extruder.core.{StringWriter, _}
import extruder.data.PathElement
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

  implicit def metricsStringWriter[F[_], S <: Settings](implicit F: Applicative[F]): StringWriter[F, S, Metrics] =
    new StringWriter[F, S, Metrics] {
      override def write(path: List[PathElement], settings: S, value: String): F[Metrics] =
        F.pure(Metrics.status(settings.pathElementsAsStrings(path), value))
    }

  implicit def traversableThrowableEncoder[F[_]: Applicative: ExtruderErrors, FF[A] <: TraversableOnce[A], S <: Settings]: EncoderT[
    F,
    S,
    FF[Throwable],
    Metrics
  ] = EncoderT.make[F, S, FF[Throwable], Metrics] { (path, settings, values) =>
    MetricKey[F](settings.pathElementsAsStrings(path), MetricType.Gauge)
      .map(key => Metrics.single(key, Coproduct[Numbers](values.size)))
  }

  implicit def traversableEncoder[F[_]: Applicative, T, FF[A] <: TraversableOnce[A], S](
    implicit encoder: EncoderT[F, S, T, Metrics],
    monoid: Monoid[Metrics]
  ): EncoderT[F, S, FF[T], Metrics] =
    EncoderT.make[F, S, FF[T], data.Metrics] { (path, settings, values) =>
      Traverse[List].traverse(values.toList)(encoder.write(path, settings, _)).map(monoid.combineAll)
    }

  implicit def numericMapEncoder[F[_]: Applicative: ExtruderErrors, K: Show, V, S <: Settings](
    implicit inj: Inject[Numbers, V]
  ): EncoderT[F, S, Map[K, V], Metrics] =
    EncoderT.make[F, S, Map[K, V], Metrics] { (path, settings, values) =>
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, K, V](newPath, values, (k, _) => MetricKey[F, K](newPath, k, None))
    }

  implicit def numericMapEncoderObjectKey[F[_]: Monad: ExtruderErrors, K <: Product with Serializable, T, S <: MetricSettings](
    implicit inj: Inject[Numbers, T],
    enc: EncoderT[F, Settings, K, Map[String, String]],
    lp: LowPriority
  ): EncoderT[F, S, Map[K, T], Metrics] =
    EncoderT.make[F, S, Map[K, T], Metrics] { (path, settings, values) =>
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, K, T](newPath, values, (k, _) => MetricKey[F, K](newPath, settings.mapEncoderSettings, k, None))
    }

  implicit def numericMapEncoderMapKey[F[_]: Applicative: ExtruderErrors, KK: Show, KV: Show, T, S <: Settings](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, Map[Map[KK, KV], T], Metrics] =
    EncoderT.make[F, S, Map[Map[KK, KV], T], Metrics] { (path, settings, values) =>
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, Map[KK, KV], T](newPath, values, (dims, _) => MetricKey[F, KK, KV](newPath, dims, None))
    }

  implicit def numericMapEncoderTupleKey[F[_]: Applicative: ExtruderErrors, KK: Show, KV: Show, T, S <: Settings](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, Map[(KK, KV), T], Metrics] =
    EncoderT.make[F, S, Map[(KK, KV), T], Metrics] { (path, settings, values) =>
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, (KK, KV), T](newPath, values, (dim, _) => MetricKey[F, KK, KV](newPath, dim, None))
    }

  implicit def metricValuesEncoder[F[_]: Applicative: ExtruderErrors, K: Show, T, V[A] <: MetricValue[A], S <: Settings](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, MetricValues[V, K, T], Metrics] =
    EncoderT.make[F, S, MetricValues[V, K, T], Metrics] { (path, settings, mv) =>
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, K, V[T], T](newPath, mv.values, (k, v) => MetricKey[F, K](newPath, k, Some(v.metricType)), _.value)
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
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, K, V[T], T](
        newPath,
        mv.values,
        (k, v) => MetricKey[F, K](newPath, settings.mapEncoderSettings, k, Some(v.metricType)),
        _.value
      )
    }

  implicit def metricValuesMapKey[F[_]: Applicative: ExtruderErrors, KK, KV, T, V[A] <: MetricValue[A], S <: Settings](
    implicit inj: Inject[Numbers, T],
    showK: Show[KK],
    showKK: Show[KV]
  ): EncoderT[F, S, MetricValues[V, Map[KK, KV], T], Metrics] =
    EncoderT.make[F, S, MetricValues[V, Map[KK, KV], T], Metrics] { (path, settings, mv) =>
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, Map[KK, KV], V[T], T](
        newPath,
        mv.values,
        (dims, v) => MetricKey[F, KK, KV](newPath, dims, Some(v.metricType)),
        _.value
      )
    }

  implicit def metricValuesTupleKey[F[_]: Applicative: ExtruderErrors, KK, KV, T, V[A] <: MetricValue[A], S <: Settings](
    implicit inj: Inject[Numbers, T],
    showKK: Show[KK],
    showKV: Show[KV]
  ): EncoderT[F, S, MetricValues[V, (KK, KV), T], Metrics] =
    EncoderT.make[F, S, MetricValues[V, (KK, KV), T], Metrics] { (path, settings, mv) =>
      val newPath = settings.pathElementsAsStrings(path)
      makeKeys[F, (KK, KV), V[T], T](newPath, mv.values, {
        case (k, v) => MetricKey[F, KK, KV](newPath, k, Some(v.metricType))
      }, _.value)
    }

  implicit def numericEncoder[F[_]: Applicative: ExtruderErrors, T, S <: Settings](
    implicit inj: Inject[Numbers, T]
  ): EncoderT[F, S, T, Metrics] =
    EncoderT.make[F, S, T, Metrics] { (path, settings, value) =>
      MetricKey[F](settings.pathElementsAsStrings(path), None)
        .map(key => Metrics.single(key, Coproduct[Numbers](value)))
    }

  implicit def metricValueEncoder[F[_]: Applicative: ExtruderErrors, T, V[A] <: MetricValue[A], S <: Settings](
    implicit inj: Inject[Numbers, T],
    refute: Refute[EncoderT[F, S, V[T], Metrics]]
  ): EncoderT[F, S, V[T], Metrics] = EncoderT.make[F, S, V[T], Metrics] { (path, settings, mv) =>
    MetricKey[F](settings.pathElementsAsStrings(path), Some(mv.metricType))
      .map(key => Metrics.single(key, Coproduct[Numbers](mv.value)))
  }

  implicit def timerEncoder[F[_]: Applicative: ExtruderErrors, S <: Settings]: EncoderT[F, S, TimerValue[Long], Metrics] =
    EncoderT.make[F, S, TimerValue[Long], Metrics] { (path, settings, tv) =>
      val v = if (tv.isFinished) tv else tv.checkpoint()
      MetricKey[F](settings.pathElementsAsStrings(path), Some(MetricType.Timer))
        .map(key => Metrics.single(key, Coproduct[Numbers](v.value)))
    }

  implicit def durationEncoder[F[_]: Applicative: ExtruderErrors, S <: Settings]: EncoderT[
    F,
    S,
    FiniteDuration,
    Metrics
  ] =
    EncoderT.make[F, S, FiniteDuration, Metrics] { (path, settings, dur) =>
      MetricKey[F](settings.pathElementsAsStrings(path), Some(MetricType.Timer))
        .map(key => Metrics.single(key, Coproduct[Numbers](dur.toMillis)))
    }

}
