package extruder.metrics

import cats.instances.list._
import cats.syntax.functor._
import cats.{Applicative, Monoid, Traverse}
import extruder.core._
import extruder.metrics.data._
import extruder.metrics.syntax.timer._
import shapeless.ops.coproduct.Inject
import shapeless.{Coproduct, LowPriority, Refute}

import scala.concurrent.duration.FiniteDuration

trait MetricEncoders
    extends PrimitiveEncoders
    with StringMapEncoders
    with DerivedEncoders
    with Encoders
    with EncodeTypes {
  override type EncodeData = Metrics
  override type Sett <: MetricSettings

  override protected def monoid: Monoid[EncodeData] = new Monoid[EncodeData] {
    override def empty: EncodeData = Map.empty
    override def combine(x: EncodeData, y: EncodeData): EncodeData =
      y.foldLeft(x) {
        case (acc, (k, v)) => acc + (k -> acc.get(k).fold(v)(Numbers.add(_, v)))
      }
  }

  override protected def writeValue[F[_]](path: List[String], settings: Sett, value: String)(
    implicit F: EncEff[F]
  ): F[EncodeData] =
    F.pure(Metrics.status(path, value))

  implicit def traversableThrowableEncoder[F[_]: EncEff: ExtruderErrors, FF[A] <: TraversableOnce[A]]: EncT[F, FF[
    Throwable
  ]] = mkEncoder { (path, _, values) =>
    MetricKey[F](path, MetricType.Gauge).map(key => Metrics.single(key, Coproduct[Numbers](values.size)))
  }

  implicit def traversableEncoder[F[_]: EncEff, T, FF[A] <: TraversableOnce[A]](
    implicit encoder: EncT[F, T]
  ): EncT[F, FF[T]] =
    mkEncoder[F, FF[T]] { (path, settings, values) =>
      Traverse[List].traverse(values.toList)(encoder.write(path, settings, _)).map(monoid.combineAll)
    }

  implicit def numericMapEncoder[F[_]: EncEff: ExtruderErrors, K: Show, T](
    implicit inj: Inject[Numbers, T]
  ): EncT[F, Map[K, T]] =
    mkEncoder[F, Map[K, T]] { (path, _, values) =>
      makeKeys[F, K, T](path, values, (k, _) => MetricKey[F, K](path, k, None))
    }

  implicit def numericMapEncoderObjectKey[F[_]: EncEff: ExtruderErrors, K <: Product with Serializable, T](
    implicit inj: Inject[Numbers, T],
    enc: MapEncoder[F, K],
    lp: LowPriority
  ): EncT[F, Map[K, T]] =
    mkEncoder[F, Map[K, T]] { (path, settings, values) =>
      makeKeys[F, K, T](path, values, (k, _) => MetricKey[F, K](path, settings.mapEncoderSettings, k, None))
    }

  implicit def numericMapEncoderMapKey[F[_]: EncEff: ExtruderErrors, KK: Show, KV: Show, T](
    implicit inj: Inject[Numbers, T]
  ): EncT[F, Map[Map[KK, KV], T]] =
    mkEncoder[F, Map[Map[KK, KV], T]] { (path, _, values) =>
      makeKeys[F, Map[KK, KV], T](path, values, (dims, _) => MetricKey[F, KK, KV](path, dims, None))
    }

  implicit def numericMapEncoderTupleKey[F[_]: EncEff: ExtruderErrors, KK: Show, KV: Show, T](
    implicit inj: Inject[Numbers, T]
  ): EncT[F, Map[(KK, KV), T]] =
    mkEncoder[F, Map[(KK, KV), T]] { (path, _, values) =>
      makeKeys[F, (KK, KV), T](path, values, (dim, _) => MetricKey[F, KK, KV](path, dim, None))
    }

  implicit def metricValuesEncoder[F[_]: EncEff: ExtruderErrors, K: Show, T, V[A] <: MetricValue[A]](
    implicit inj: Inject[Numbers, T]
  ): EncT[F, MetricValues[V, K, T]] =
    mkEncoder[F, MetricValues[V, K, T]] { (path, _, mv) =>
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
  )(implicit inj: Inject[Numbers, T]): F[Metrics] =
    Traverse[List]
      .traverse(values.toList) {
        case (k, v) =>
          makeKey(k, v).map { key =>
            Metrics.single(key, Coproduct[Numbers](makeValue(v)))
          }
      }
      .map(monoid.combineAll(_))

  implicit def metricValuesObjectKey[F[_]: EncEff: ExtruderErrors, K <: Product with Serializable, T, V[A] <: MetricValue[
    A
  ]](implicit inj: Inject[Numbers, T], enc: MapEncoder[F, K], lp: LowPriority): EncT[F, MetricValues[V, K, T]] =
    mkEncoder[F, MetricValues[V, K, T]] { (path, settings, mv) =>
      makeKeys[F, K, V[T], T](
        path,
        mv.values,
        (k, v) => MetricKey[F, K](path, settings.mapEncoderSettings, k, Some(v.metricType)),
        _.value
      )
    }

  implicit def metricValuesMapKey[F[_]: EncEff: ExtruderErrors, KK, KV, T, V[A] <: MetricValue[A]](
    implicit inj: Inject[Numbers, T],
    showK: Show[KK],
    showKK: Show[KV]
  ): EncT[F, MetricValues[V, Map[KK, KV], T]] = mkEncoder[F, MetricValues[V, Map[KK, KV], T]] { (path, _, mv) =>
    makeKeys[F, Map[KK, KV], V[T], T](
      path,
      mv.values,
      (dims, v) => MetricKey[F, KK, KV](path, dims, Some(v.metricType)),
      _.value
    )
  }

  implicit def metricValuesTupleKey[F[_]: EncEff: ExtruderErrors, KK, KV, T, V[A] <: MetricValue[A]](
    implicit inj: Inject[Numbers, T],
    showKK: Show[KK],
    showKV: Show[KV]
  ): EncT[F, MetricValues[V, (KK, KV), T]] = mkEncoder[F, MetricValues[V, (KK, KV), T]] { (path, _, mv) =>
    makeKeys[F, (KK, KV), V[T], T](path, mv.values, {
      case (k, v) => MetricKey[F, KK, KV](path, k, Some(v.metricType))
    }, _.value)
  }

  implicit def numericEncoder[F[_]: EncEff: ExtruderErrors, T](implicit inj: Inject[Numbers, T]): EncT[F, T] =
    mkEncoder[F, T] { (path, _, value) =>
      MetricKey[F](path, None).map(key => Metrics.single(key, Coproduct[Numbers](value)))
    }

  implicit def metricValueEncoder[F[_]: EncEff: ExtruderErrors, T, V[A] <: MetricValue[A]](
    implicit inj: Inject[Numbers, T],
    refute: Refute[EncT[F, V[T]]]
  ): EncT[F, V[T]] = mkEncoder[F, V[T]] { (path, _, mv) =>
    MetricKey[F](path, Some(mv.metricType)).map(key => Metrics.single(key, Coproduct[Numbers](mv.value)))
  }

  implicit def timerEncoder[F[_]: EncEff: ExtruderErrors]: EncT[F, TimerValue[Long]] = mkEncoder[F, TimerValue[Long]] {
    (path, _, tv) =>
      val v = if (tv.isFinished) tv else tv.checkpoint()
      MetricKey[F](path, Some(MetricType.Timer)).map(key => Metrics.single(key, Coproduct[Numbers](v.value)))
  }

  implicit def durationEncoder[F[_]: EncEff: ExtruderErrors]: EncT[F, FiniteDuration] = mkEncoder { (path, _, dur) =>
    MetricKey[F](path, Some(MetricType.Timer)).map(key => Metrics.single(key, Coproduct[Numbers](dur.toMillis)))
  }

}

trait MetricSettings extends Settings {
  val mapEncoderSettings: Settings = new Settings {
    override val includeClassNameInPath: Boolean = false
    override def pathToString(path: List[String]): String = path.mkString(".")
  }

  override def pathToString(path: List[String]): String = path.map(snakeCaseTransformation).mkString(".")
}

trait MetricEncoder[F[_], S, T] extends EncoderT[F, S, T, Metrics]
