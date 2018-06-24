package extruder.core

import cats.Monoid

trait Encoders { self: EncodeTypes =>
  protected def monoid: Monoid[EncodeData]

  protected def mkEncoder[F[_], T](f: (List[String], Sett, T) => F[EncodeData]): Enc[F, T]

  def apply[F[_], T](implicit encoder: Enc[F, T]): Enc[F, T] = encoder
}

trait Encode { self: EncodeTypes =>
  protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: EncodeData)(
    implicit F: Eff[F]
  ): F[OutputData]

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param value   the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F       implicit [[Eff]] instance
    * @tparam A type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[A](value: A)(implicit encoder: Enc[Validation, A], F: Eff[Validation]): Validation[OutputData] =
    encode[Validation, A](defaultSettings, value)

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param value   the value to be encoded
    * @param settings  optional data source settings
    * @param encoder implicit [[Encoder]] instance
    * @param F       implicit [[Eff]] instance
    * @tparam A type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[A](
    settings: Sett,
    value: A
  )(implicit encoder: Enc[Validation, A], F: Eff[Validation]): Validation[OutputData] =
    encode[Validation, A](settings, value)

  /** Encode the given value as data in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param namespace namespace within the data source to place encoded values
    * @param value     the value to be encoded
    * @param encoder   implicit [[Encoder]] instance
    * @param F         implicit [[Eff]] instance
    * @tparam A type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[A](
    namespace: List[String],
    value: A
  )(implicit encoder: Enc[Validation, A], F: Eff[Validation]): Validation[OutputData] =
    encode[Validation, A](namespace, defaultSettings, value)

  /** Encode the given value as data in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param namespace namespace within the data source to place encoded values
    * @param settings  optional data source settings
    * @param value     the value to be encoded
    * @param encoder   implicit [[Encoder]] instance
    * @param F         implicit [[Eff]] instance
    * @tparam A type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[A](namespace: List[String], settings: Sett, value: A)(
    implicit encoder: Enc[Validation, A],
    F: Eff[Validation]
  ): Validation[OutputData] =
    encode[Validation, A](namespace, settings, value)

  /** Encode the given value as data, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param value   the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F       implicit [[Eff]] instance
    * @tparam A type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[F[_], A](value: A)(implicit encoder: Enc[F, A], F: Eff[F]): F[OutputData] =
    encode[F, A](defaultSettings, value)

  /** Encode the given value as data, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param value   the value to be encoded
    * @param settings  optional data source settings
    * @param encoder implicit [[Encoder]] instance
    * @param F       implicit [[Eff]] instance
    * @tparam A type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[F[_], A](settings: Sett, value: A)(implicit encoder: Enc[F, A], F: Eff[F]): F[OutputData] =
    encode[F, A](List.empty, settings, value)

  /** Encode the given value as data in a given namespace, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param value     the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder   implicit [[Encoder]] instance
    * @param F         implicit [[Eff]] instance
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam A type to be encoded
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[F[_], A](namespace: List[String], value: A)(implicit encoder: Enc[F, A], F: Eff[F]): F[OutputData] =
    encode[F, A](namespace, defaultSettings, value)

  /** Encode the given value as data in a given namespace, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param value     the value to be encoded
    * @param settings  optional data source settings
    * @param namespace namespace within the data source to place encoded values
    * @param encoder   implicit [[Encoder]] instance
    * @param F         implicit [[Eff]] instance
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam A type to be encoded
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[F[_], A](namespace: List[String], settings: Sett, value: A)(
    implicit encoder: Enc[F, A],
    F: Eff[F]
  ): F[OutputData] =
    F.flatMap(encoder.write(namespace, settings, value))(finalizeOutput[F](namespace, settings, _))

}

trait Encoder[F[_], S, T, O] {
  def write(path: List[String], settings: S, in: T): F[O]
}

trait EncoderRefute[T]

trait EncodeTypes extends DataSource {
  type EncodeData
  type Enc[F[_], T] <: Encoder[F, Sett, T, EncodeData]
  type EncRefute[T] <: EncoderRefute[T]
}
