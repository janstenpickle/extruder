package extruder.core

import cats.{FlatMap, Functor}
import cats.syntax.functor._
import cats.syntax.flatMap._
import extruder.instances.DecoderTInstances

/**
  * Reads value `A` from input `I`, wrapping result in functor `F`.
  *
  * @tparam F Functor in which to wrap return type
  * @tparam S Settings to use when decoding
  * @tparam A Decoded type
  * @tparam I Input data
  */
trait DecoderT[F[_], S, A, I] {
  def read(path: List[String], settings: S, default: Option[A], input: I): F[A]

  def imap[B](f: A => B)(g: B => A)(implicit F: Functor[F]): DecoderT[F, S, B, I] = DecoderT.make[F, S, B, I] {
    (path, settings, default, input) =>
      read(path, settings, default.map(g), input).map(f)
  }

  def imapResult[B](f: A => F[B])(g: B => A)(implicit F: FlatMap[F]): DecoderT[F, S, B, I] = DecoderT.make[F, S, B, I] {
    (path, settings, default, input) =>
      read(path, settings, default.map(g), input).flatMap(f)
  }
}

object DecoderT
    extends DecoderTInstances
    with ParserDecoderTInstances
    with MapDecoderTInstances
    with DerivedDecoderTInstances
    with GenericDecoderTInstances
    with CombinedDecoderTInstances {
  def make[F[_]: Functor, S, A, C](f: (List[String], S, Option[A], C) => F[A]): DecoderT[F, S, A, C] =
    new DecoderT[F, S, A, C] {
      override def read(path: List[String], settings: S, default: Option[A], input: C): F[A] =
        f(path, settings, default, input)
    }

  def apply[F[_], S, A, C](implicit decoder: DecoderT[F, S, A, C]): DecoderT[F, S, A, C] = decoder
}
