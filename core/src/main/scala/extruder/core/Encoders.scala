package extruder.core

import cats.Monoid

trait Encoders { self: EncodeTypes =>
  protected def monoid: Monoid[EncodeData]

  protected def mkEncoder[F[_], T](f: (List[String], T) => F[EncodeData]): Enc[F, T]

  def apply[F[_], T](implicit encoder: Enc[F, T]): Enc[F, T] = encoder
}

trait Encode { self: EncodeTypes =>
  protected def finalizeOutput[F[_]](namespace: List[String], inter: EncodeData)(
    implicit F: ExtruderEffect[F],
    hints: Hint
  ): F[OutputData]

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F implicit [[ExtruderEffect]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[T](
    value: T
  )(implicit encoder: Enc[Validation, T], F: ExtruderEffect[Validation], hints: Hint): Validation[OutputData] =
    encode[T, Validation](value)

  /** Encode the given value as data in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param namespace namespace within the data source to place encoded values
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F implicit [[ExtruderEffect]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[T](
    namespace: List[String],
    value: T
  )(implicit encoder: Enc[Validation, T], F: ExtruderEffect[Validation], hints: Hint): Validation[OutputData] =
    encode[T, Validation](namespace, value)

  /** Encode the given value as data, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F implicit [[ExtruderEffect]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[T, F[_]](value: T)(implicit encoder: Enc[F, T], F: ExtruderEffect[F], hints: Hint): F[OutputData] =
    encode(List.empty, value)

  /** Encode the given value as data in a given namespace, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param F implicit [[ExtruderEffect]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[T, F[_]](
    namespace: List[String],
    value: T
  )(implicit encoder: Enc[F, T], F: ExtruderEffect[F], hints: Hint): F[OutputData] =
    F.flatMap(encoder.write(namespace, value))(finalizeOutput[F](namespace, _))

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F implicit [[ExtruderEffect]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @throws Throwable any error encountered during encoding
    * @return The value encoded as [[OutputData]]
    */
  def encodeUnsafe[T](
    value: T
  )(implicit encoder: Enc[Validation, T], F: ExtruderEffect[Validation], hints: Hint): OutputData =
    encodeUnsafe[T](List.empty, value)

  /** Encode the given value as data in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param F implicit [[ExtruderEffect]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @throws Throwable any error encountered during encoding
    * @return The value encoded as [[OutputData]]
    */
  def encodeUnsafe[T](
    namespace: List[String],
    value: T
  )(implicit encoder: Enc[Validation, T], F: ExtruderEffect[Validation], hints: Hint): OutputData =
    encode[T](namespace, value).fold(errs => throw errorsToThrowable(errs), identity)
}

trait Encoder[F[_], T, O] {
  def write(path: List[String], in: T): F[O]
}

trait EncodeTypes extends DataSource {
  type EncodeData
  type Enc[F[_], T] <: Encoder[F, T, EncodeData]
}
