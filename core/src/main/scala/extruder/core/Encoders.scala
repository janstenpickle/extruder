package extruder.core

import cats.Monoid

trait Encoders { self: EncodeTypes =>
  protected def monoid: Monoid[EncodeData]

  protected def mkEncoder[F[_], T](f: (List[String], T) => F[EncodeData]): Enc[F, T]

  def apply[F[_], T](implicit encoder: Enc[F, T]): Enc[F, T] = encoder
}

trait Encode { self: EncodeTypes =>
  protected def finalizeOutput[F[_]](namespace: List[String], inter: EncodeData)(
    implicit F: Eff[F],
    hints: Hint
  ): F[OutputData]

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value   the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F       implicit [[Eff]] instance
    * @param hints   implicit [[Hints]] instance
    * @tparam A type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[A](
    value: A
  )(implicit encoder: Enc[Validation, A], F: Eff[Validation], hints: Hint): Validation[OutputData] =
    encode[Validation, A](value)

  /** Encode the given value as data in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param namespace namespace within the data source to place encoded values
    * @param value     the value to be encoded
    * @param encoder   implicit [[Encoder]] instance
    * @param F         implicit [[Eff]] instance
    * @param hints     implicit [[Hints]] instance
    * @tparam A type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[A](
    namespace: List[String],
    value: A
  )(implicit encoder: Enc[Validation, A], F: Eff[Validation], hints: Hint): Validation[OutputData] =
    encode[Validation, A](namespace, value)

  /** Encode the given value as data, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value   the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param F       implicit [[Eff]] instance
    * @param hints   implicit [[Hints]] instance
    * @tparam A type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[F[_], A](value: A)(implicit encoder: Enc[F, A], F: Eff[F], hints: Hint): F[OutputData] =
    encode[F, A](List.empty, value)

  /** Encode the given value as data in a given namespace, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value     the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder   implicit [[Encoder]] instance
    * @param F         implicit [[Eff]] instance
    * @param hints     implicit [[Hints]] instance
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam A type to be encoded
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[F[_], A](
    namespace: List[String],
    value: A
  )(implicit encoder: Enc[F, A], F: Eff[F], hints: Hint): F[OutputData] =
    F.flatMap(encoder.write(namespace, value))(finalizeOutput[F](namespace, _))
}

trait Encoder[F[_], T, O] {
  def write(path: List[String], in: T): F[O]
}

trait EncoderRefute[T]

trait EncodeTypes extends DataSource {
  type EncodeData
  type Enc[F[_], T] <: Encoder[F, T, EncodeData]
  type EncRefute[T] <: EncoderRefute[T]
}
