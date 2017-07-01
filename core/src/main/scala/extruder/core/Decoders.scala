package extruder.core

import cats.effect.IO

trait Decoders { self: DecodeTypes =>
  protected def mkDecoder[F[_], T](f: (List[String], Option[T], DecodeData) => IO[F[T]]): Dec[F, T]

  def apply[F[_], T](implicit decoder: Dec[F, T]): Dec[F, T] = decoder
}

trait Decode { self: DecodeTypes =>
  protected def prepareInput[F[_], E](namespace: List[String], input: InputData)(
    implicit AE: ExtruderApplicativeError[F, E],
    hints: Hint
  ): IO[F[DecodeData]]

  /** Decode the specified type from given data source
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): Validation[T] =
    decode[T, Validation, ValidationErrors](input)

  /** Decode the specified type from given data source in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the data source to look for values
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](namespace: List[String], input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): Validation[T] =
    decode[T, Validation, ValidationErrors](namespace, input)

  /** Decode the specified type from given data source, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param input data source
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
    input: InputData
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[T] =
    decode(List.empty, input)

  /** Decode the specified type from given data source in a given namespace, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the data source to look for values
    * @param input data source
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
    namespace: List[String],
    input: InputData
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[T] =
    AE.attemptIO(decodeIO(namespace, input))

  /** Decode the specified type from given data source
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): T =
    decodeUnsafe[T](List.empty, input)

  /** Decode the specified type from given data source in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the data source to look for values
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](namespace: List[String], input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): T =
    decode[T](namespace, input).fold(errs => throw errorsToThrowable(errs), identity)

  /** Decode the specified type from given data source
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[Validation[T]] =
    decodeIO[T, Validation, ValidationErrors](input)

  /** Decode the specified type from given data source in a given namespace
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the data source to look for values
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](namespace: List[String], input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): IO[Validation[T]] =
    decodeIO[T, Validation, ValidationErrors](namespace, input)

  /** Decode the specified type from given data source, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    * @param input data source
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
    input: InputData
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): IO[F[T]] =
    decodeIO(List.empty, input)

  /** Decode the specified type from given data source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the data source to look for values
    * @param input data source
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
    namespace: List[String],
    input: InputData
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): IO[F[T]] =
    FM.flatMap(prepareInput(namespace, input))(decoder.read(namespace, None, _))

  /** Decode the specified type from given data source
    * Side effects in reading from the data source are encoded in `M`
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_]](input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[Validation[T]] =
    decodeAsync[T, M, Validation, ValidationErrors](input)

  /** Decode the specified type from given data source in a given namespace
    * Side effects in reading from the data source are encoded in `M`
    * @param namespace namespace within the data source to look for values
    * @param input data source
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_]](namespace: List[String], input: InputData)(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[Validation[T]] =
    decodeAsync[T, M, Validation, ValidationErrors](namespace, input)

  /** Decode the specified type from given data source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in `M`
    * @param input data source
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
  def decodeAsync[T, M[_], F[_], E](input: InputData)(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[T]] =
    decodeAsync[T, M, F, E](List.empty, input)

  /** Decode the specified type from given data source, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in `M`
    * @param namespace namespace within the data source to look for values
    * @param input data source
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
  def decodeAsync[T, M[_], F[_], E](namespace: List[String], input: InputData)(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[T]] =
    IOC.fromIO(decodeIO[T, F, E](namespace, input))

  /** Create a table of parameters as they should appear in the data source
    *
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @param hints implicit [[Hints]] instance for formatting parameter paths
    * @tparam T type to be represented
    * @return a String table of parameters
    */
  def parameters[T](implicit params: Parameters[T], hints: Hint): String =
    parameters(List.empty[String])

  /** Create a table of parameters as they should appear in the data source in a given namespace
    *
    * @param namespace namespace within the data source
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @param hints implicit [[Hints]] instance for formatting parameter paths
    * @tparam T type to be represented
    * @return a String table of parameters
    */
  def parameters[T](namespace: List[String])(implicit params: Parameters[T], hints: Hint): String =
    FormatParameters.asTable[T](hints.pathToString, namespace)
}

trait DecodeFromDefaultSource { self: Decode with DecodeTypes =>
  protected def loadInput: IO[InputData]

  /** Decode the specified type from a default data source
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): Validation[T] =
    decode[T, Validation, ValidationErrors]

  /** Decode the specified type from a default data source in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the data source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[T](namespace: List[String])(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): Validation[T] =
    decode[T, Validation, ValidationErrors](namespace)

  /** Decode the specified type from a default data source, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
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
    decode[T, F, E](List.empty)

  /** Decode the specified type from a default data source in a given namespace, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the data source to look for values
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
    namespace: List[String]
  )(implicit decoder: Dec[F, T], AE: ExtruderApplicativeError[F, E], FM: IOFlatMap[F], hints: Hint): F[T] =
    AE.attemptIO(decodeIO[T, F, E](namespace))

  /** Decode the specified type from a default data source
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): T =
    decodeUnsafe[T](List.empty)

  /** Decode the specified type from a default data source in a given namespace
    * If the data source is asynchronous by nature this method will wait for the duration specified in `hints` for decoding to timeout
    * @param namespace namespace within the data source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @throws Throwable any error encountered during decoding
    * @return the requested type
    */
  def decodeUnsafe[T](namespace: List[String])(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): T =
    decode[T](namespace).fold(errs => throw errorsToThrowable(errs), identity)

  /** Decode the specified type from a default data source
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): IO[Validation[T]] =
    decodeIO[T, Validation, ValidationErrors]

  /** Decode the specified type from a default data source in a given namespace
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the data source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the IO monad
    */
  def decodeIO[T](namespace: List[String])(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    hints: Hint
  ): IO[Validation[T]] =
    decodeIO[T, Validation, ValidationErrors](namespace)

  /** Decode the specified type from a default data source, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
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
    decodeIO[T, F, E](List.empty)

  /** Decode the specified type a default data source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in the [[cats.effect.IO]] monad
    * @param namespace namespace within the data source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam E error type (e.g. `Exception`)
    * @return The requested type wrapped in the target monad, wrapped in the IO monad
    */
  def decodeIO[T, F[_], E](namespace: List[String])(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    dis: `decode without input disambiguator`.type,
    hints: Hint
  ): IO[F[T]] =
    loadInput.flatMap(decodeIO[T, F, E](namespace, _))

  /** Decode the specified type from a default data source
    * Side effects in reading from the data source are encoded in `M`
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
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    IOC: IOConvert[M],
    dis: `decode disambiguator`.type,
    hints: Hint
  ): M[Validation[T]] =
    decodeAsync[T, M, Validation, ValidationErrors]

  /** Decode the specified type from a default data source in a given namespace
    * Side effects in reading from the data source are encoded in `M`
    * @param namespace namespace within the data source to look for values
    * @param decoder implicit [[Decoder]] instance
    * @param AE implicit [[ExtruderApplicativeError]] instance
    * @param FM implicit [[IOFlatMap]] instance
    * @param IOC implicit [[IOConvert]] instance
    * @param hints implicit [[Hints]] instance
    * @tparam T type to be decoded
    * @tparam M monad representing side effects
    * @return Either the requested type or a non-empty list of validation errors, wrapped in the specified effect monad
    */
  def decodeAsync[T, M[_]](namespace: List[String])(
    implicit decoder: Dec[Validation, T],
    AE: ExtruderApplicativeError[Validation, ValidationErrors],
    FM: IOFlatMap[Validation],
    IOC: IOConvert[M],
    dis: `decode without input disambiguator`.type,
    hints: Hint
  ): M[Validation[T]] =
    decodeAsync[T, M, Validation, ValidationErrors](namespace)

  /** Decode the specified type from a default data source in a given namespace, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in `M`
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
    decodeAsync[T, M, F, E](List.empty)

  /** Decode the specified type from a default data source, wrapping the result in specified target monad
    * Side effects in reading from the data source are encoded in `M`
    * @param namespace namespace within the data source to look for values
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
  def decodeAsync[T, M[_], F[_], E](namespace: List[String])(
    implicit decoder: Dec[F, T],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    IOC: IOConvert[M],
    hints: Hint
  ): M[F[T]] =
    IOC.fromIO(decodeIO[T, F, E](namespace))
}

trait Decoder[F[_], T, C] {
  def read(path: List[String], default: Option[T], input: C): IO[F[T]]
}

trait DecodeTypes extends DataSource {
  type DecodeData
  type Dec[F[_], T] <: Decoder[F, T, DecodeData]
}
