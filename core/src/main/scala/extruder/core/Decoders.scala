package extruder.core

import cats.effect.IO

trait Decoders { self: DecodeTypes =>
  protected def mkDecoder[F[_], T](f: (Seq[String], Option[T], DecodeConfig) => IO[F[T]]): Dec[F, T]

  def apply[F[_], T](implicit decoder: Dec[F, T]): Dec[F, T] = decoder
}

trait Decode { self: DecodeTypes =>
  protected def prepareConfig[F[_], E](namespace: Seq[String], config: InputConfig)(
    implicit AE: ExtruderApplicativeError[F, E],
    hints: Hint
  ): IO[F[DecodeConfig]]

  /** Decode the specified type from given configuration source
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): ConfigValidation[T] =
    decode[T, ConfigValidation, ValidationErrors](config)

  /** Decode the specified type from given configuration source in a given namespace
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the configuration source to look for values
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](namespace: Seq[String], config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): ConfigValidation[T] =
    decode[T, ConfigValidation, ValidationErrors](namespace, config)

  /** Decode the specified type from given configuration source, wrapping the result in specified target monad
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad
    */
  def decode[T, F[_], E](
    config: InputConfig
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[T] =
    decode(Seq.empty, config)

  /** Decode the specified type from given configuration source in a given namespace, wrapping the result in specified target monad
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the configuration source to look for values
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad
    */
  def decode[T, F[_], E](
    namespace: Seq[String],
    config: InputConfig
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[T] =
    AE.attemptIO(decodeIO(namespace, config))

  /** Decode the specified type from given configuration source
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): T =
    decodeUnsafe[T](Seq.empty, config)

  /** Decode the specified type from given configuration source in a given namespace
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the configuration source to look for values
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](namespace: Seq[String], config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): T =
    decode[T](namespace, config).fold(errs => throw errorsToThrowable(errs), identity)

  /** Decode the specified type from given configuration source
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[ConfigValidation[T]] =
    decodeIO[T, ConfigValidation, ValidationErrors](config)

  /** Decode the specified type from given configuration source in a given namespace
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the configuration source to look for values
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](namespace: Seq[String], config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[ConfigValidation[T]] =
    decodeIO[T, ConfigValidation, ValidationErrors](namespace, config)

  /** Decode the specified type from given configuration source, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the IO monad
    */
  def decodeIO[T, F[_], E](
    config: InputConfig
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): IO[F[T]] =
    decodeIO(Seq.empty, config)

  /** Decode the specified type from given configuration source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the configuration source to look for values
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the IO monad
    */
  def decodeIO[T, F[_], E](
    namespace: Seq[String],
    config: InputConfig
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): IO[F[T]] =
    FM.flatMap(prepareConfig(namespace, config))(decoder.read(namespace, None, _))

  /** Decode the specified type from given configuration source
    * Side effects in reading from the configuration source are encoded in `M`
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_]](config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[ConfigValidation[T]] =
    decodeAsync[T, M, ConfigValidation, ValidationErrors](config)

  /** Decode the specified type from given configuration source in a given namespace
    * Side effects in reading from the configuration source are encoded in `M`
    * @param namespace namespace within the configuration source to look for values
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_]](namespace: Seq[String], config: InputConfig)(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[ConfigValidation[T]] =
    decodeAsync[T, M, ConfigValidation, ValidationErrors](namespace, config)

  /** Decode the specified type from given configuration source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in `M`
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the IO monad
    */
  def decodeAsync[T, M[_], F[_], E](config: InputConfig)(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[T]] =
    decodeAsync[T, M, F, E](Seq.empty, config)

  /** Decode the specified type from given configuration source, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in `M`
    * @param namespace namespace within the configuration source to look for values
    * @param config configuration source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the IO monad
    */
  def decodeAsync[T, M[_], F[_], E](namespace: Seq[String], config: InputConfig)(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[T]] =
    IOC.fromIO(decodeIO[T, F, E](namespace, config))

  /** Create a table of parameters as they should appear in the configuration source
    *
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @param hints implicit [[Hints]] instance for formatting parameter paths
    * @tparam T type to be represented
    * @return a String table of parameters
    */
  def parameters[T](implicit params: Parameters[T], hints: Hint): String =
    parameters(Seq.empty[String])

  /** Create a table of parameters as they should appear in the configuration source in a given namespace
    *
    * @param namespace namespace within the configuration source
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @param hints implicit [[Hints]] instance for formatting parameter paths
    * @tparam T type to be represented
    * @return a String table of parameters
    */
  def parameters[T](namespace: Seq[String])(implicit params: Parameters[T], hints: Hint): String =
    FormatParameters.asTable[T](hints.pathToString, namespace)
}

trait DecodeFromDefaultConfig { self: Decode with DecodeTypes =>
  protected def loadConfig: IO[InputConfig]

  /** Decode the specified type from a default configuration source
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): ConfigValidation[T] =
    decode[T, ConfigValidation, ValidationErrors]

  /** Decode the specified type from a default configuration source in a given namespace
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the configuration source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](namespace: Seq[String])(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): ConfigValidation[T] =
    decode[T, ConfigValidation, ValidationErrors](namespace)

  /** Decode the specified type from a default configuration source, wrapping the result in specified target monad
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad
    */
  def decode[T, F[_], E](
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    hints: Hint
  ): F[T] =
    decode[T, F, E](Seq.empty)

  /** Decode the specified type from a default configuration source in a given namespace, wrapping the result in specified target monad
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the configuration source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad
    */
  def decode[T, F[_], E](
    namespace: Seq[String]
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[T] =
    AE.attemptIO(decodeIO[T, F, E](namespace))

  /** Decode the specified type from a default configuration source
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): T =
    decodeUnsafe[T](Seq.empty)

  /** Decode the specified type from a default configuration source in a given namespace
    * If the configuration source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the configuration source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](namespace: Seq[String])(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): T =
    decode[T](namespace).fold(errs => throw errorsToThrowable(errs), identity)

  /** Decode the specified type from a default configuration source
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): IO[ConfigValidation[T]] =
    decodeIO[T, ConfigValidation, ValidationErrors]

  /** Decode the specified type from a default configuration source in a given namespace
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the configuration source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](namespace: Seq[String])(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    hints: Hint
  ): IO[ConfigValidation[T]] =
    decodeIO[T, ConfigValidation, ValidationErrors](namespace)

  /** Decode the specified type from a default configuration source, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the IO monad
    */
  def decodeIO[T, F[_], E](
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[F[T]] =
    decodeIO[T, F, E](Seq.empty)

  /** Decode the specified type a default configuration source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the configuration source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the IO monad
    */
  def decodeIO[T, F[_], E](namespace: Seq[String])(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    dis: `decode without config disambiguator`.type,
    hints: Hint
  ): IO[F[T]] =
    loadConfig.flatMap(decodeIO[T, F, E](namespace, _))

  /** Decode the specified type from a default configuration source
    * Side effects in reading from the configuration source are encoded in `M`
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_]](
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[ConfigValidation[T]] =
    decodeAsync[T, M, ConfigValidation, ValidationErrors]

  /** Decode the specified type from a default configuration source in a given namespace
    * Side effects in reading from the configuration source are encoded in `M`
    * @param namespace namespace within the configuration source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_]](namespace: Seq[String])(
    implicit decoder: Dec[ConfigValidation, T],
    AE: ExtruderApplicativeError[ConfigValidation, ValidationErrors],
    FM: IOFlatMap[ConfigValidation],
    IOC: IOConvert[M],
    dis: `decode without config disambiguator`.type,
    hints: Hint
  ): M[ConfigValidation[T]] =
    decodeAsync[T, M, ConfigValidation, ValidationErrors](namespace)

  /** Decode the specified type from a default configuration source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in `M`
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_], F[_], E](
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[T]] =
    decodeAsync[T, M, F, E](Seq.empty)

  /** Decode the specified type from a default configuration source, wrapping the result in specified target monad
    * Side effects in reading from the configuration source are encoded in `M`
    * @param namespace namespace within the configuration source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_], F[_], E](namespace: Seq[String])(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[T]] =
    IOC.fromIO(decodeIO[T, F, E](namespace))
}

trait Decoder[F[_], T, C] {
  def read(path: Seq[String], default: Option[T], config: C): IO[F[T]]
}

trait DecodeTypes extends ConfigSource {
  type DecodeConfig
  type Dec[F[_], T] <: Decoder[F, T, DecodeConfig]
}
