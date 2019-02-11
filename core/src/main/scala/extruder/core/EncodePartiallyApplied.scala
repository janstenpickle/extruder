package extruder.core

import cats.syntax.flatMap._
import cats.FlatMap
import cats.data.Ior

trait EncodePartiallyApplied[F[_], S, E, O] {
  def apply[A](
    namespace: List[String],
    settings: S,
    value: A
  )(implicit F: FlatMap[F], encoder: EncoderT[F, S, A, E], transform: Transform[F, S, E, O]): F[O] =
    encoder.write(namespace, settings, value).flatMap(transform.run(namespace, settings, _))

  def apply[A](
    settings: S,
    value: A
  )(implicit F: FlatMap[F], encoder: EncoderT[F, S, A, E], transform: Transform[F, S, E, O]): F[O] =
    apply(List.empty, settings, value)

  def combine[S1, E1, O1]: EncodePartiallyApplied[F, (S1, S), Ior[E1, E], Ior[O1, O]] =
    new EncodePartiallyApplied[F, (S1, S), Ior[E1, E], Ior[O1, O]] {}

  def combine(
    other: Encode with DataSource
  ): EncodePartiallyApplied[F, (other.Sett, S), Ior[other.EncodeData, E], Ior[other.OutputData, O]] =
    new EncodePartiallyApplied[F, (other.Sett, S), Ior[other.EncodeData, E], Ior[other.OutputData, O]] {}
}

trait EncodePartiallyAppliedWithDefaultSettings[F[_], S, E, O] extends EncodePartiallyApplied[F, S, E, O] { outer =>
  protected def defaultSettings: S

  def apply[A](
    namespace: List[String],
    value: A
  )(implicit F: FlatMap[F], encoder: EncoderT[F, S, A, E], transform: Transform[F, S, E, O]): F[O] =
    apply(namespace, defaultSettings, value)

  def apply[A](
    value: A
  )(implicit F: FlatMap[F], encoder: EncoderT[F, S, A, E], transform: Transform[F, S, E, O]): F[O] =
    apply(List.empty, defaultSettings, value)

  def combine[S1, E1, O1](settings: S1): EncodePartiallyAppliedWithDefaultSettings[F, (S1, S), Ior[E1, E], Ior[O1, O]] =
    new EncodePartiallyAppliedWithDefaultSettings[F, (S1, S), Ior[E1, E], Ior[O1, O]] {
      override protected def defaultSettings: (S1, S) = (settings, outer.defaultSettings)
    }

  def combine[S1, E1, O1](
    other: EncodePartiallyAppliedWithDefaultSettings[F, S1, E1, O1]
  ): EncodePartiallyAppliedWithDefaultSettings[F, (S1, S), Ior[E1, E], Ior[O1, O]] =
    new EncodePartiallyAppliedWithDefaultSettings[F, (S1, S), Ior[E1, E], Ior[O1, O]] {
      override protected def defaultSettings: (S1, S) = (other.defaultSettings, outer.defaultSettings)
    }

  override def combine(
    other: Encode with DataSource
  ): EncodePartiallyAppliedWithDefaultSettings[F, (other.Sett, S), Ior[other.EncodeData, E], Ior[other.OutputData, O]] =
    new EncodePartiallyAppliedWithDefaultSettings[F, (other.Sett, S), Ior[other.EncodeData, E], Ior[
      other.OutputData,
      O
    ]] {
      override protected def defaultSettings: (other.Sett, S) = (other.defaultSettings, outer.defaultSettings)
    }
}
