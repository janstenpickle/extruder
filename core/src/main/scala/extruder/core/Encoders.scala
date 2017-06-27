package extruder.core

import cats.Monoid
import cats.effect.IO

trait Encoders { self: EncodeTypes =>
  protected def monoid: Monoid[EncodeConfig]

  protected def mkEncoder[F[_], T](f: (Seq[String], T) => IO[F[EncodeConfig]]): Enc[F, T]

  def apply[F[_], T](implicit encoder: Enc[F, T]): Enc[F, T] = encoder
}

trait Encode { self: EncodeTypes =>
  protected def finalizeConfig[F[_], E](namespace: Seq[String], inter: EncodeConfig)(
    implicit AE: ExtruderApplicativeError[F, E],
    hints: Hint
  ): IO[F[OutputConfig]]

  /** Encode the given value as configuration
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputConfig]] or a non-empty list of errors
    */
  def encode[T](value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    hints: Hint
  ): ConfigValidation[OutputConfig] =
    encode[T, ConfigValidation, ValidationErrors](value)

  /** Encode the given value as configuration in a given namespace
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the configuration source to place encoded values
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputConfig]] or a non-empty list of errors
    */
  def encode[T](namespace: Seq[String], value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): ConfigValidation[OutputConfig] =
    encode[T, ConfigValidation, ValidationErrors](namespace, value)

  /** Encode the given value as configuration, wrapping the result in specified target monad
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return the value encoded as [[OutputConfig]] wrapped in the target monad
    */
  def encode[T, F[_], E](
    value: T
  )(implicit encoder: Enc[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[OutputConfig] =
    encode(Seq.empty, value)

  /** Encode the given value as configuration in a given namespace, wrapping the result in specified target monad
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param value the value to be encoded
    * @param namespace namespace within the configuration source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return the value encoded as [[OutputConfig]] wrapped in the target monad
    */
  def encode[T, F[_], E](
    namespace: Seq[String],
    value: T
  )(implicit encoder: Enc[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[OutputConfig] =
    AE.attemptIO(encodeIO(namespace, value))

  /** Encode the given value as configuration
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @throws Throwable any error encountered during encoding
    * @return The value encoded as [[OutputConfig]]
    */
  def encodeUnsafe[T](value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    hints: Hint
  ): OutputConfig =
    encodeUnsafe[T](Seq.empty, value)

  /** Encode the given value as configuration in a given namespace
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param value the value to be encoded
    * @param namespace namespace within the configuration source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @throws Throwable any error encountered during encoding
    * @return The value encoded as [[OutputConfig]]
    */
  def encodeUnsafe[T](namespace: Seq[String], value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    hints: Hint
  ): OutputConfig =
    encode[T](namespace, value).fold(errs => throw errorsToThrowable(errs), identity)

  /** Encode the given value as configuration in a given namespace
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param value the value to be encoded
    * @param namespace namespace within the configuration source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputConfig]] or a non-empty list of errors wrapped in the [[cats.effect.IO]] monad
    */
  def encodeIO[T](namespace: Seq[String], value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[ConfigValidation[OutputConfig]] =
    encodeIO[T, ConfigValidation, ValidationErrors](namespace, value)

  /** Encode the given value as configuration
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @return Either the value encoded as [[OutputConfig]] or a non-empty list of errors wrapped in the [[cats.effect.IO]] monad
    */
  def encodeIO[T](value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[ConfigValidation[OutputConfig]] =
    encodeIO[T, ConfigValidation, ValidationErrors](value)

  /** Encode the given value as configuration
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The value encoded as [[OutputConfig]] wrapped in the target monad, wrapped in the IO monad
    */
  def encodeIO[T, F[_], E](value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    hints: Hint
  ): IO[F[OutputConfig]] =
    encodeIO(Seq.empty, value)

  /** Encode the given value as configuration in a given namespace
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param value the value to be encoded
    * @param namespace namespace within the configuration source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be encoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The value encoded as [[OutputConfig]] wrapped in the target monad, wrapped in the IO monad
    */
  def encodeIO[T, F[_], E](namespace: Seq[String], value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    hints: Hint
  ): IO[F[OutputConfig]] =
    FM.flatMap(encoder.write(namespace, value))(finalizeConfig[F, E](namespace, _))

  /** Encode the given value as configuration
    * Side effects in reading from the configuration source are encoded in `M`
    * @param value the value to be encoded
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the value encoded as [[OutputConfig]] or a non-empty list of errors wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_]](value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[ConfigValidation[OutputConfig]] =
    encodeAsync[T, M, ConfigValidation, ValidationErrors](value)

  /** Encode the given value as configuration in a given namespace
    * Side effects in reading from the configuration source are encoded in `M`
    * @param value the value to be encoded
    * @param namespace namespace within the configuration source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the value encoded as [[OutputConfig]] or a non-empty list of errors wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_]](namespace: Seq[String], value: T)(
    implicit encoder: Enc[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[ConfigValidation[OutputConfig]] =
    encodeAsync[T, M, ConfigValidation, ValidationErrors](namespace, value)

  /** Encode the given value as configuration
    * Side effects in reading from the configuration source are encoded in `M`
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
    * @return The value encoded as [[OutputConfig]] wrapped in the target monad, wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_], F[_], E](value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[OutputConfig]] =
    encodeAsync[T, M, F, E](Seq.empty, value)

  /** Encode the given value as configuration in a given namespace
    * Side effects in reading from the configuration source are encoded in `M`
    * @param value the value to be encoded
    * @param namespace namespace within the configuration source to place encoded values
    * @param encoder implicit [[Encoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The value encoded as [[OutputConfig]] wrapped in the target monad, wrapped in the specified effect monad
    */
  def encodeAsync[T, M[_], F[_], E](namespace: Seq[String], value: T)(
    implicit encoder: Enc[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[OutputConfig]] =
    IOC.fromIO(encodeIO[T, F, E](namespace, value))
}

trait Encoder[F[_], T, O] {
  def write(path: Seq[String], in: T): IO[F[O]]
}

trait EncodeTypes extends ConfigSource {
  type EncodeConfig
  type Enc[F[_], T] <: Encoder[F, T, EncodeConfig]
}
