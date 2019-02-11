package extruder.core

import cats.FlatMap
import cats.syntax.flatMap._

trait DecodePartiallyApplied[F[_], A, S, D, I] {
  def apply(namespace: List[String], settings: S, input: I)(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    transform: Transform[F, S, I, D]
  ): F[A] = transform.run(namespace, settings, input).flatMap(decoder.read(namespace, settings, None, _))

  def apply(
    settings: S,
    input: I
  )(implicit decoder: DecoderT[F, S, A, D], F: FlatMap[F], transform: Transform[F, S, I, D]): F[A] =
    apply(List.empty, settings, input)

  def apply(namespace: List[String], settings: S)(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    loadInput.load.flatMap(apply(namespace, settings, _))

  def apply(settings: S)(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    loadInput.load.flatMap(apply(settings, _))

  def combine[S1, D1, I1]: DecodePartiallyApplied[F, A, (S1, S), (D1, D), (I1, I)] =
    new DecodePartiallyApplied[F, A, (S1, S), (D1, D), (I1, I)] {}

  def combine(
    other: Decode with DataSource
  ): DecodePartiallyApplied[F, A, (other.Sett, S), (other.DecodeData, D), (other.InputData, I)] =
    new DecodePartiallyApplied[F, A, (other.Sett, S), (other.DecodeData, D), (other.InputData, I)] {}

}

trait DecodePartiallyAppliedWithDefaultSettings[F[_], A, S, D, I] extends DecodePartiallyApplied[F, A, S, D, I] {
  outer =>
  protected def defaultSettings: S

  def apply(
    namespace: List[String],
    input: I
  )(implicit decoder: DecoderT[F, S, A, D], F: FlatMap[F], transform: Transform[F, S, I, D]): F[A] =
    apply(namespace, defaultSettings, input)

  def apply(input: I)(implicit decoder: DecoderT[F, S, A, D], F: FlatMap[F], transform: Transform[F, S, I, D]): F[A] =
    apply(List.empty, defaultSettings, input)

  def apply(namespace: List[String])(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    loadInput.load.flatMap(apply(namespace, _))

  def apply()(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    apply(List.empty)

  def combine[S1, D1, I1](settings: S1): DecodePartiallyAppliedWithDefaultSettings[F, A, (S1, S), (D1, D), (I1, I)] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, (S1, S), (D1, D), (I1, I)] {
      override protected def defaultSettings: (S1, S) = (settings, outer.defaultSettings)
    }

  def combine[S1, D1, I1](
    other: DecodePartiallyAppliedWithDefaultSettings[F, A, S1, D1, I1]
  ): DecodePartiallyAppliedWithDefaultSettings[F, A, (S1, S), (D1, D), (I1, I)] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, (S1, S), (D1, D), (I1, I)] {
      override protected def defaultSettings: (S1, S) = (other.defaultSettings, outer.defaultSettings)
    }

  override def combine(
    other: Decode with DataSource
  ): DecodePartiallyAppliedWithDefaultSettings[F, A, (other.Sett, S), (other.DecodeData, D), (other.InputData, I)] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, (other.Sett, S), (other.DecodeData, D), (other.InputData, I)] {
      override protected def defaultSettings: (other.Sett, S) = (other.defaultSettings, outer.defaultSettings)
    }
}
