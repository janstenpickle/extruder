package extruder.laws

import cats.kernel.laws.IsEq
import cats.laws._
import cats.{Applicative, Monoid}
import extruder.core.{DecoderT, ExtruderErrors, Settings}

trait DecoderLaws[F[_], S <: Settings, E, D, O] {
  implicit def F: Applicative[F]
  def settings: S
  def outMonoid: Monoid[O]
  def errors: ExtruderErrors[F]

  def decodeEmpty[A](path: List[String])(implicit decoder: DecoderT[F, S, A, O]): IsEq[F[A]] =
    decoder.read(path, settings, None, outMonoid.empty) <-> errors.missing(
      s"Could not find value at '${settings.pathToString(path)}' and no default available"
    )

  def decodeDefault[A](a: A, path: List[String])(implicit decoder: DecoderT[F, S, A, O]): IsEq[F[A]] =
    decoder.read(path, settings, Some(a), outMonoid.empty) <-> F.pure(a)
}

object DecoderLaws {
  def apply[F[_]: Applicative: ExtruderErrors, S <: Settings, E, D, O: Monoid](s: S): DecoderLaws[F, S, E, D, O] =
    new DecoderLaws[F, S, E, D, O] {
      override def F: Applicative[F] = Applicative[F]
      override def settings: S = s
      override def outMonoid: Monoid[O] = Monoid[O]
      override def errors: ExtruderErrors[F] = ExtruderErrors[F]
    }
}
