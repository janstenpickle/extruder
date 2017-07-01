package extruder.core

import cats.Monoid
import cats.effect.IO

trait Encoders { self: EncodeTypes =>
  protected def monoid: Monoid[EncodeData]

  protected def mkEncoder[F[_], T](f: (List[String], T) => IO[F[EncodeData]]): Enc[F, T]

  def apply[F[_], T](implicit encoder: Enc[F, T]): Enc[F, T] = encoder
}

trait Encode { self: EncodeTypes =>
  protected def finalizeOutput[F[_], E](namespace: List[String], inter: EncodeData)(
    implicit AE: ExtruderApplicativeError[F, E],
    hints: Hint
  ): IO[F[OutputData]]

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[T](value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    hints: Hint
  ): Validation[OutputData] =
    encode[T, Validation, ValidationErrors](value)

  /** Encode the given value as data in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param namespace namespace within the data source to place encoded values
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode[T](namespace: List[String], value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): Validation[OutputData] =
    encode[T, Validation, ValidationErrors](namespace, value)

  /** Encode the given value as data, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[T, F[_], E](
    value: T
  )(implicit encoder: Enc[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[OutputData] =
    encode(List.empty, value)

  /** Encode the given value as data in a given namespace, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return the value encoded as [[OutputData]] wrapped in the target monad
    */
  def encode[T, F[_], E](
    namespace: List[String],
    value: T
  )(implicit encoder: Enc[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[OutputData] =
    AE.attemptIO(encodeIO(namespace, value))

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @throws Throwable any error encountered during encoding
    * @return The value encoded as [[OutputData]]
    */
  def encodeUnsafe[T](value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    hints: Hint
  ): OutputData =
    encodeUnsafe[T](List.empty, value)

  /** Encode the given value as data in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @throws Throwable any error encountered during encoding
    * @return The value encoded as [[OutputData]]
    */
  def encodeUnsafe[T](namespace: List[String], value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    hints: Hint
  ): OutputData =
    encode[T](namespace, value).fold(errs => throw errorsToThrowable(errs), identity)

  /** Encode the given value as data in a given namespace
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors wrapped in the [[cats.effect.IO]] monad
    */
  def encodeIO[T](namespace: List[String], value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[Validation[OutputData]] =
    encodeIO[T, Validation, ValidationErrors](namespace, value)

  /** Encode the given value as data
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors wrapped in the [[cats.effect.IO]] monad
    */
  def encodeIO[T](value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[Validation[OutputData]] =
    encodeIO[T, Validation, ValidationErrors](value)

  /** Encode the given value as data
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The value encoded as [[OutputData]] wrapped in the target monad, wrapped in the IO monad
    */
  def encodeIO[T, F[_], E](value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    hints: Hint
  ): IO[F[OutputData]] =
    encodeIO(List.empty, value)

  /** Encode the given value as data in a given namespace
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The value encoded as [[OutputData]] wrapped in the target monad, wrapped in the IO monad
    */
  def encodeIO[T, F[_], E](namespace: List[String], value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    hints: Hint
  ): IO[F[OutputData]] =
    FM.flatMap(encoder.write(namespace, value))(finalizeOutput[F, E](namespace, _))

  /** Encode the given value as data
    * Side effects in reading from the data source are encoded in `M`
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_]](value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[Validation[OutputData]] =
    encodeAsync[T, M, Validation, ValidationErrors](value)

  /** Encode the given value as data in a given namespace
    * Side effects in reading from the data source are encoded in `M`
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_]](namespace: List[String], value: T)(
    implicit encoder: Enc[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[Validation[OutputData]] =
    encodeAsync[T, M, Validation, ValidationErrors](namespace, value)

  /** Encode the given value as data
    * Side effects in reading from the data source are encoded in `M`
    *
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The value encoded as [[OutputData]] wrapped in the target monad, wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_], F[_], E](value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[OutputData]] =
    encodeAsync[T, M, F, E](List.empty, value)

  /** Encode the given value as data in a given namespace
    * Side effects in reading from the data source are encoded in `M`
    *
    * @param value the value to be encoded
    * @param namespace namespace within the data source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The value encoded as [[OutputData]] wrapped in the target monad, wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_], F[_], E](namespace: List[String], value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[OutputData]] =
    IOC.fromIO(encodeIO[T, F, E](namespace, value))
}

trait Encoder[F[_], T, O] {
  def write(path: List[String], in: T): IO[F[O]]
}

trait EncodeTypes extends DataSource {
  type EncodeData
  type Enc[F[_], T] <: Encoder[F, T, EncodeData]
}
