package extruder.core

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{FlatMap, Functor}
import extruder.data.PathElement
import extruder.instances.DecoderInstances

/**
  * Reads value `A` from input `I`, wrapping result in functor `F`.
  *
  * @tparam F Functor in which to wrap return type
  * @tparam S Settings to use when decoding
  * @tparam A Decoded type
  * @tparam I Input data
  */
trait Decoder[F[_], S, A, I] {
  def read(path: List[PathElement], settings: S, default: Option[A], input: I): F[A]

  def imap[B](f: A => B)(g: B => A)(implicit F: Functor[F]): Decoder[F, S, B, I] = Decoder.make[F, S, B, I] {
    (path, settings, default, input) =>
      read(path, settings, default.map(g), input).map(f)
  }

  def imapResult[B](f: A => F[B])(g: B => A)(implicit F: FlatMap[F]): Decoder[F, S, B, I] = Decoder.make[F, S, B, I] {
    (path, settings, default, input) =>
      read(path, settings, default.map(g), input).flatMap(f)
  }
}

object Decoder
    extends DecoderInstances
    with ParserDecoderInstances
    with MapDecoderInstances
    with DerivedDecoderInstances
    with GenericDecoderInstances
    with CombinedDecoderInstances {
  def make[F[_], S, A, C](f: (List[PathElement], S, Option[A], C) => F[A]): Decoder[F, S, A, C] =
    new Decoder[F, S, A, C] {
      override def read(path: List[PathElement], settings: S, default: Option[A], input: C): F[A] =
        f(path, settings, default, input)
    }

  def apply[F[_], S, A, C](implicit decoder: Decoder[F, S, A, C]): Decoder[F, S, A, C] = decoder
}
