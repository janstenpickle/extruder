package extruder.laws

import cats.{Monad, Monoid}
import extruder.core._
import cats.laws._
import extruder.data.PathElement
import cats.syntax.flatMap._
import cats.syntax.functor._

trait EncoderDecoderDerivedLaws[F[_], S <: Settings, E, D, O] extends EncoderDecoderLaws[F, S, E, D, O] {
  implicit def hasValue: HasValue[F, S, O]

  private def encodePrepare(path: List[PathElement], input: F[E]): F[O] =
    for {
      i <- input
      f <- finalise.run(path, settings, i)
      o <- prepare.run(path, settings, f)
    } yield o

  def eitherLeftEncodeDecode[A, B](path: List[PathElement], a: A)(
    implicit aEncoder: Encoder[F, S, A, E],
    bEncoder: Encoder[F, S, B, E],
    aDecoder: Decoder[F, S, A, O],
    bDecoder: Decoder[F, S, B, O]
  ): IsEq[F[Either[A, B]]] = {
    val eitherEncoder: Encoder[F, S, Either[A, B], E] = Encoder[F, S, Either[A, B], E]
    val eitherDecoder: Decoder[F, S, Either[A, B], O] = Decoder[F, S, Either[A, B], O]

    (for {
      left <- encodePrepare(path, eitherEncoder.write(path, settings, Left(a)))
      l <- eitherDecoder.read(path, settings, None, left)
    } yield l) <-> F.pure[Either[A, B]](Left(a))
  }

  def eitherRightEncodeDecode[A, B](path: List[PathElement], b: B)(
    implicit aEncoder: Encoder[F, S, A, E],
    bEncoder: Encoder[F, S, B, E],
    aDecoder: Decoder[F, S, A, O],
    bDecoder: Decoder[F, S, B, O]
  ): IsEq[F[Either[A, B]]] = {
    val eitherEncoder: Encoder[F, S, Either[A, B], E] = Encoder[F, S, Either[A, B], E]
    val eitherDecoder: Decoder[F, S, Either[A, B], O] = Decoder[F, S, Either[A, B], O]

    (for {
      right <- encodePrepare(path, eitherEncoder.write(path, settings, Right(b)))
      r <- eitherDecoder.read(path, settings, None, right)
    } yield r) <-> F.pure[Either[A, B]](Right(b))
  }

  def eitherLeftDefaultDecode[A, B](
    path: List[PathElement],
    a: A
  )(implicit aDecoder: Decoder[F, S, A, O], bDecoder: Decoder[F, S, B, O]): IsEq[F[Either[A, B]]] = {
    val eitherDecoder: Decoder[F, S, Either[A, B], O] = Decoder[F, S, Either[A, B], O]

    eitherDecoder.read(path, settings, Some(Left(a)), outMonoid.empty) <-> F.pure(Left(a))
  }

  def eitherRightDefaultDecode[A, B](
    path: List[PathElement],
    b: B
  )(implicit aDecoder: Decoder[F, S, A, O], bDecoder: Decoder[F, S, B, O]): IsEq[F[Either[A, B]]] = {
    val eitherDecoder: Decoder[F, S, Either[A, B], O] = Decoder[F, S, Either[A, B], O]

    eitherDecoder.read(path, settings, Some(Right(b)), outMonoid.empty) <-> F.pure(Right(b))
  }
}

object EncoderDecoderDerivedLaws {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](s: S)(
    implicit fin: Transform[F, S, E, D],
    prep: Transform[F, S, D, O],
    hv: HasValue[F, S, O]
  ): EncoderDecoderDerivedLaws[F, S, E, D, O] =
    new EncoderDecoderDerivedLaws[F, S, E, D, O] {
      override def F: Monad[F] = Monad[F]
      override def settings: S = s
      override def finalise: Transform[F, S, E, D] = fin
      override def prepare: Transform[F, S, D, O] = prep
      override def monoid: Monoid[E] = Monoid[E]
      override def outMonoid: Monoid[O] = Monoid[O]
      override def errors: ExtruderErrors[F] = ExtruderErrors[F]
      override def hasValue: HasValue[F, S, O] = hv
    }
}
