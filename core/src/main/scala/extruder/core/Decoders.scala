package extruder.core

import cats.{Id, MonadError}

trait Decoders { self: DecodeTypes =>
  protected def mkDecoder[F[_], T](f: (List[String], Sett, Option[T], DecodeData) => F[T]): DecT[F, T]

  protected def selectOption[F[_], A](path: List[String], settings: Sett, primary: Option[A], secondary: Option[A])(
    implicit F: DecEff[F],
    error: ExtruderErrors[F]
  ): F[A] =
    (primary, secondary) match {
      case (None, None) =>
        error.missing(s"Could not find value at '${settings.pathToString(path)}' and no default available")
      case (None, Some(value)) => F.pure(value)
      case (Some(value), _) => F.pure(value)
    }

  def apply[F[_], T](implicit decoder: DecT[F, T]): DecT[F, T] = decoder
}

trait Decode { self: DecodeTypes =>
  protected def prepareInput[F[_]](namespace: List[String], settings: Sett, input: InputData)(
    implicit F: DecEff[F]
  ): F[DecodeData]

  type PartiallyApplied[F[_], A] <: DecodePartiallyApplied[F, A]

  protected def mkPartiallApplied[F[_], A]: PartiallyApplied[F, A] =
    new DecodePartiallyApplied[F, A] {}.asInstanceOf[PartiallyApplied[F, A]]

  /** Decode the specified type from given data source
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param input   data source
    * @param decoder implicit [[DecoderT]] instance
    * @param F       implicit [[DecEff]] instance
    * @tparam A type to be decoded
    * @return Either the requested type or a non-empty list of validation errors
    */
  def decode[A]: PartiallyApplied[DecDefault, A] = mkPartiallApplied[DecDefault, A]

  /** Decode the specified type from given data source, wrapping the result in specified target monad
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param input   data source
    * @param decoder implicit [[DecoderT]] instance
    * @param F       implicit [[DecEff]] instance
    * @tparam F target monad (e.g. [[scala.util.Try]])
    * @tparam A type to be decoded
    * @return The requested type wrapped in the target monad
    */
  def decodeF[F[_], A]: PartiallyApplied[F, A] = mkPartiallApplied[F, A]

  /** Create a table of parameters as they should appear in the data source
    *
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @tparam A type to be represented
    * @return a String table of parameters
    */
  def parameters[A](implicit params: Parameters[A]): String =
    parameters[A](defaultSettings)

  /** Create a table of parameters as they should appear in the data source
    *
    * @param settings settings class used when decoding input data
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @tparam A type to be represented
    * @return a String table of parameters
    */
  def parameters[A](settings: Sett)(implicit params: Parameters[A]): String =
    parameters[A](List.empty[String], settings)

  /** Create a table of parameters as they should appear in the data source in a given namespace
    *
    * @param namespace namespace within the data source
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @tparam A type to be represented
    * @return a String table of parameters
    */
  def parameters[A](namespace: List[String])(implicit params: Parameters[A]): String =
    parameters[A](namespace, defaultSettings)

  /** Create a table of parameters as they should appear in the data source in a given namespace
    *
    * @param namespace namespace within the data source
    * @param settings settings class used when decoding input data
    * @param params implicit [[Parameters]] type class as a representation of `T`
    * @tparam A type to be represented
    * @return a String table of parameters
    */
  def parameters[A](namespace: List[String], settings: Sett)(implicit params: Parameters[A]): String =
    FormatParameters.asTable[A](settings.pathToString, namespace)

  trait DecodePartiallyApplied[F[_], A] {
    def apply(namespace: List[String], settings: Sett, input: InputData)(
      implicit decoder: DecT[F, A],
      F: DecEff[F]
    ): F[A] = F.flatMap(prepareInput(namespace, settings, input))(decoder.read(namespace, settings, None, _))

    def apply(settings: Sett, input: InputData)(implicit decoder: DecT[F, A], F: DecEff[F]): F[A] =
      apply(List.empty, settings, input)

    def apply(namespace: List[String], input: InputData)(implicit decoder: DecT[F, A], F: DecEff[F]): F[A] =
      apply(namespace, defaultSettings, input)

    def apply(input: InputData)(implicit decoder: DecT[F, A], F: DecEff[F]): F[A] =
      apply(List.empty, defaultSettings, input)
  }
}

trait DecodeFromDefaultSource { self: Decode with DecodeTypes =>
  override type PartiallyApplied[F[_], A] = DecodeDefaultPartiallyApplied[F, A]

  protected def loadInput[F[_]](implicit F: DecEff[F]): F[InputData]

  override protected def mkPartiallApplied[F[_], A]: PartiallyApplied[F, A] =
    new DecodeDefaultPartiallyApplied[F, A] {}

  trait DecodeDefaultPartiallyApplied[F[_], A] extends DecodePartiallyApplied[F, A] {
    def apply(namespace: List[String], settings: Sett)(implicit decoder: DecT[F, A], F: DecEff[F]): F[A] =
      F.flatMap(loadInput)(apply(namespace, settings, _))

    def apply(namespace: List[String])(implicit decoder: DecT[F, A], F: DecEff[F]): F[A] =
      apply(namespace, defaultSettings)

    def apply(implicit decoder: DecT[F, A], F: DecEff[F]): F[A] =
      apply(List.empty, defaultSettings)
  }
}

trait DecoderT[F[_], S, T, C] {
  def read(path: List[String], settings: S, default: Option[T], input: C): F[T]
}

trait DecoderRefute[T]

trait DecodeTypes extends DataSource {
  type DecodeData
  type DecDefault[A]
  type DecEff[F[_]] <: MonadError[F, Throwable]
  type DecT[F[_], T] <: DecoderT[F, Sett, T, DecodeData]
  type Dec[T] = DecT[Id, T]
  type DecRefute[T] <: DecoderRefute[T]
}
