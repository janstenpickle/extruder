package extruder.core

import cats.FlatMap
import cats.syntax.flatMap._
import extruder.data.PathElement

trait DecodePartiallyApplied[F[_], A, S, D, I] {

  /**
    * Decode a value A from input data I
    *
    * @param namespace namespace under which the data is stored
    * @param settings settings used when decoding
    * @param input input data
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply(
    namespace: List[String],
    settings: S,
    input: I
  )(implicit decoder: DecoderT[F, S, A, D], F: FlatMap[F], transform: Transform[F, S, I, D]): F[A] = {
    val newNamespace = namespace.map(PathElement.Standard)
    transform.run(newNamespace, settings, input).flatMap(decoder.read(newNamespace, settings, None, _))
  }

  /**
    * Decode a value A from input data I
    *
    * @param settings settings used when decoding
    * @param input input data
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply(
    settings: S,
    input: I
  )(implicit decoder: DecoderT[F, S, A, D], F: FlatMap[F], transform: Transform[F, S, I, D]): F[A] =
    apply(List.empty, settings, input)

  /**
    * Decode a value A from input data I
    *
    * @param namespace namespace under which the data is stored
    * @param settings settings used when decoding
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param loadInput implicit instance for loading data externally
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply(namespace: List[String], settings: S)(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    loadInput.load.flatMap(apply(namespace, settings, _))

  /**
    * Decode a value A from input data I
    *
    * @param settings settings used when decoding
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param loadInput implicit instance for loading data externally
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply(settings: S)(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    loadInput.load.flatMap(apply(settings, _))

  /**
    * Combine decoder with another decoder instance
    *
    * @tparam S1 other source's settings type
    * @tparam I1 other source's input data type
    * @tparam D1 other source's decode data type
    * @return a new combined instance of DecodePartiallyApplied
    */
  def combine[S1, I1, D1]: DecodePartiallyApplied[F, A, (S, S1), (D, D1), (I, I1)] =
    new DecodePartiallyApplied[F, A, (S, S1), (D, D1), (I, I1)] {}

  /**
    * Combine decoder with another decoder instance
    *
    * @param other other data source from where types can be taken
    * @return a new combined instance of DecodePartiallyApplied
    */
  def combine(
    other: Decode with DataSource
  ): DecodePartiallyApplied[F, A, (S, other.Sett), (D, other.DecodeData), (I, other.InputData)] =
    new DecodePartiallyApplied[F, A, (S, other.Sett), (D, other.DecodeData), (I, other.InputData)] {}

}

trait DecodePartiallyAppliedWithDefaultSettings[F[_], A, S, D, I] extends DecodePartiallyApplied[F, A, S, D, I] {
  outer =>
  protected def defaultSettings: S

  /**
    * Decode a value A from input data I
    *
    * @param namespace namespace under which the data is stored
    * @param input input data
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply(
    namespace: List[String],
    input: I
  )(implicit decoder: DecoderT[F, S, A, D], F: FlatMap[F], transform: Transform[F, S, I, D]): F[A] =
    apply(namespace, defaultSettings, input)

  /**
    * Decode a value A from input data I
    *
    * @param input input data
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply(input: I)(implicit decoder: DecoderT[F, S, A, D], F: FlatMap[F], transform: Transform[F, S, I, D]): F[A] =
    apply(List.empty, defaultSettings, input)

  /**
    * Decode a value A from input data I
    *
    * @param namespace namespace under which the data is stored
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param loadInput implicit instance for loading data externally
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply(namespace: List[String])(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    loadInput.load.flatMap(apply(namespace, _))

  /**
    * Decode a value A from input data I
    *
    * @param decoder implicit decoder instance
    * @param F implicit flatMap instance
    * @param loadInput implicit instance for loading data externally
    * @param transform implicit transform instance to convert from input I to decode data D
    * @return decoded value A, wrapping in functor F
    */
  def apply()(
    implicit decoder: DecoderT[F, S, A, D],
    F: FlatMap[F],
    loadInput: LoadInput[F, I],
    transform: Transform[F, S, I, D]
  ): F[A] =
    apply(List.empty)

  /**
    * Combine decoder with another decoder instance
    *
    * @param settings other data source's settings to provide a default value for in DecodePartiallyAppliedWithDefaultSettings
    * @tparam S1 other source's settings type
    * @tparam I1 other source's input data type
    * @tparam D1 other source's decode data type
    * @return a new combined instance of DecodePartiallyAppliedWithDefaultSettings
    */
  def combine[S1, D1, I1](settings: S1): DecodePartiallyAppliedWithDefaultSettings[F, A, (S, S1), (D, D1), (I, I1)] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, (S, S1), (D, D1), (I, I1)] {
      override protected def defaultSettings: (S, S1) = (outer.defaultSettings, settings)
    }

  /**
    * Combine decoder with another decoder instance
    *
    * @param other other data source's DecodePartiallyAppliedWithDefaultSettings to provide a default settings value
    * @tparam S1 other source's settings type
    * @tparam I1 other source's input data type
    * @tparam D1 other source's decode data type
    * @return a new combined instance of DecodePartiallyAppliedWithDefaultSettings
    */
  def combine[S1, D1, I1](
    other: DecodePartiallyAppliedWithDefaultSettings[F, A, S1, D1, I1]
  ): DecodePartiallyAppliedWithDefaultSettings[F, A, (S, S1), (D, D1), (I, I1)] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, (S, S1), (D, D1), (I, I1)] {
      override protected def defaultSettings: (S, S1) = (outer.defaultSettings, other.defaultSettings)
    }

  /**
    * Combine decoder with another decoder instance
    *
    * @param other other data source from where types can be taken
    * @return a new combined instance of DecodePartiallyAppliedWithDefaultSettings
    */
  override def combine(
    other: Decode with DataSource
  ): DecodePartiallyAppliedWithDefaultSettings[F, A, (S, other.Sett), (D, other.DecodeData), (I, other.InputData)] =
    new DecodePartiallyAppliedWithDefaultSettings[F, A, (S, other.Sett), (D, other.DecodeData), (I, other.InputData)] {
      override protected def defaultSettings: (S, other.Sett) = (outer.defaultSettings, other.defaultSettings)
    }
}
