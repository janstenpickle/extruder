package extruder.core

import cats.{Id, Monad, Monoid}

trait Encoders { self: EncodeTypes =>
  protected def monoid: Monoid[EncodeData]

  protected def mkEncoder[F[_], T](f: (List[String], Sett, T) => F[EncodeData]): EncT[F, T]

  def apply[F[_], T](implicit encoder: EncT[F, T]): EncT[F, T] = encoder
}

trait Encode { self: EncodeTypes =>
  protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: EncodeData)(
    implicit F: EncEff[F]
  ): F[OutputData]

  /** Encode the given value as data
    * If the data source is asynchronous by nature this method will wait for the duration specified in `settings` for decoding to timeout
    *
    * @param value   the value to be encoded
    * @param encoder implicit [[EncoderT]] instance
    * @param F       implicit [[EncEff]] instance
    * @tparam A type to be encoded
    * @return Either the value encoded as [[OutputData]] or a non-empty list of errors
    */
  def encode: EncodePartiallyApplied[EncDefault] =
    new EncodePartiallyApplied[EncDefault] {}

  def encodeF[F[_]]: EncodePartiallyApplied[F] =
    new EncodePartiallyApplied[F] {}

  trait EncodePartiallyApplied[F[_]] {
    def apply[A](namespace: List[String], settings: Sett, value: A)(
      implicit encoder: EncT[F, A],
      F: EncEff[F]
    ): F[OutputData] = F.flatMap(encoder.write(namespace, settings, value))(finalizeOutput[F](namespace, settings, _))

    def apply[A](namespace: List[String], value: A)(implicit encoder: EncT[F, A], F: EncEff[F]): F[OutputData] =
      apply(namespace, defaultSettings, value)

    def apply[A](value: A)(implicit encoder: EncT[F, A], F: EncEff[F]): F[OutputData] =
      apply(List.empty, defaultSettings, value)
  }
}

trait EncoderT[F[_], S, T, O] {
  def write(path: List[String], settings: S, in: T): F[O]
}

trait EncoderRefute[T]

trait EncodeTypes extends DataSource {
  type EncodeData
  type EncDefault[A]
  type EncEff[F[_]] <: Monad[F]
  type EncT[F[_], T] <: EncoderT[F, Sett, T, EncodeData]
  type Enc[T] = EncT[Id, T]
  type EncRefute[T] <: EncoderRefute[T]
}
