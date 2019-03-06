package extruder.core

import cats.syntax.flatMap._
import cats.FlatMap
import cats.data.Ior
import extruder.data.PathElement

trait EncodePartiallyApplied[F[_], S, E, O] {

  /**
    * Encode a value A as output data O
    *
    * @param namespace namespace at which to put the value within the output
    * @param settings settings to be used when encoding
    * @param value value to encode
    * @param F implicit flatMap instance
    * @param encoder implicit encoder instance
    * @param transform implicit transform instance for to be used to convert internal encoded data type to output data
    * @tparam A type of value to be encoded
    * @return encoded data wrapped in functor F
    */
  def apply[A](
    namespace: List[String],
    settings: S,
    value: A
  )(implicit F: FlatMap[F], encoder: Encoder[F, S, A, E], transform: Transform[F, S, E, O]): F[O] = {
    val newNamespace = namespace.map(PathElement.Standard)
    encoder.write(newNamespace, settings, value).flatMap(transform.run(newNamespace, settings, _))
  }

  /**
    * Encode a value A as output data O
    *
    * @param settings settings to be used when encoding
    * @param value value to encode
    * @param F implicit flatMap instance
    * @param encoder implicit encoder instance
    * @param transform implicit transform instance for to be used to convert internal encoded data type to output data
    * @tparam A type of value to be encoded
    * @return encoded data wrapped in functor F
    */
  def apply[A](
    settings: S,
    value: A
  )(implicit F: FlatMap[F], encoder: Encoder[F, S, A, E], transform: Transform[F, S, E, O]): F[O] =
    apply(List.empty, settings, value)

  /**
    * Combine encoder with another encoder instance
    *
    * @tparam S1 other source's settings type
    * @tparam E1 other source's internal encode data type
    * @tparam O1 other source's output data type
    * @return a new combined instance of EncodePartiallyApplied
    */
  def combine[S1, E1, O1]: EncodePartiallyApplied[F, (S, S1), Ior[E, E1], Ior[O, O1]] =
    new EncodePartiallyApplied[F, (S, S1), Ior[E, E1], Ior[O, O1]] {}

  /**
    * Combine encoder with another encoder instance
    *
    * @param other other data source from which to retrieve types
    * @return a new combined instance of EncodePartiallyApplied
    */
  def combine(
    other: Encode with DataSource
  ): EncodePartiallyApplied[F, (S, other.Sett), Ior[E, other.EncodeData], Ior[O, other.OutputData]] =
    new EncodePartiallyApplied[F, (S, other.Sett), Ior[E, other.EncodeData], Ior[O, other.OutputData]] {}
}

trait EncodePartiallyAppliedWithDefaultSettings[F[_], S, E, O] extends EncodePartiallyApplied[F, S, E, O] { outer =>
  protected def defaultSettings: S

  /**
    * Encode a value A as output data O
    *
    * @param namespace namespace at which to put the value within the output
    * @param value value to encode
    * @param F implicit flatMap instance
    * @param encoder implicit encoder instance
    * @param transform implicit transform instance for to be used to convert internal encoded data type to output data
    * @tparam A type of value to be encoded
    * @return encoded data wrapped in functor F
    */
  def apply[A](
    namespace: List[String],
    value: A
  )(implicit F: FlatMap[F], encoder: Encoder[F, S, A, E], transform: Transform[F, S, E, O]): F[O] =
    apply(namespace, defaultSettings, value)

  /**
    * Encode a value A as output data O
    *
    * @param value value to encode
    * @param F implicit flatMap instance
    * @param encoder implicit encoder instance
    * @param transform implicit transform instance for to be used to convert internal encoded data type to output data
    * @tparam A type of value to be encoded
    * @return encoded data wrapped in functor F
    */
  def apply[A](value: A)(implicit F: FlatMap[F], encoder: Encoder[F, S, A, E], transform: Transform[F, S, E, O]): F[O] =
    apply(List.empty, defaultSettings, value)

  /**
    * Combine encoder with another encoder instance
    *
    * @param settings other data source's settings to provide a default value for in EncodePartiallyAppliedWithDefaultSettings
    * @tparam S1 other source's settings type
    * @tparam E1 other source's internal encode data type
    * @tparam O1 other source's output data type
    * @return a new combined instance of EncodePartiallyAppliedWithDefaultSettings
    */
  def combine[S1, E1, O1](settings: S1): EncodePartiallyAppliedWithDefaultSettings[F, (S, S1), Ior[E, E1], Ior[O, O1]] =
    new EncodePartiallyAppliedWithDefaultSettings[F, (S, S1), Ior[E, E1], Ior[O, O1]] {
      override protected def defaultSettings: (S, S1) = (outer.defaultSettings, settings)
    }

  /**
    * Combine encoder with another encoder instance
    *
    * @param other other data source's EncodePartiallyAppliedWithDefaultSettings to provide a default settings value
    * @tparam S1 other source's settings type
    * @tparam E1 other source's internal encode data type
    * @tparam O1 other source's output data type
    * @return a new combined instance of EncodePartiallyAppliedWithDefaultSettings
    */
  def combine[S1, E1, O1](
    other: EncodePartiallyAppliedWithDefaultSettings[F, S1, E1, O1]
  ): EncodePartiallyAppliedWithDefaultSettings[F, (S, S1), Ior[E, E1], Ior[O, O1]] =
    new EncodePartiallyAppliedWithDefaultSettings[F, (S, S1), Ior[E, E1], Ior[O, O1]] {
      override protected def defaultSettings: (S, S1) = (outer.defaultSettings, other.defaultSettings)
    }

  /**
    * Combine encoder with another encoder instance
    *
    * @param other other data source from where types can be taken
    * @return a new combined instance of EncodePartiallyAppliedWithDefaultSettings
    */
  override def combine(
    other: Encode with DataSource
  ): EncodePartiallyAppliedWithDefaultSettings[F, (S, other.Sett), Ior[E, other.EncodeData], Ior[O, other.OutputData]] =
    new EncodePartiallyAppliedWithDefaultSettings[F, (S, other.Sett), Ior[E, other.EncodeData], Ior[
      O,
      other.OutputData
    ]] {
      override protected def defaultSettings: (S, other.Sett) = (outer.defaultSettings, other.defaultSettings)
    }
}
