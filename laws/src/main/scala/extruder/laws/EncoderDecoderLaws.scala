package extruder.laws

import cats.data.{Ior, IorT}
import cats.kernel.laws.IsEq
import cats.laws._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, Monoid}
import extruder.core._
import shapeless.Lazy

trait EncoderDecoderLaws[F[_], S <: Settings, E, D, O] extends DecoderLaws[F, S, E, D, O] {
  implicit def F: Monad[F]
  def settings: S
  implicit def finalise: Transform[F, S, E, D]
  implicit def prepare: Transform[F, S, D, O]
  def monoid: Monoid[E]
  implicit def errors: ExtruderErrors[F]

  private def test[A](path: List[String], a: A)(
    encoder: EncoderT[F, S, A, E],
    fin: Transform[F, S, E, D],
    prep: Transform[F, S, D, O],
    decoder: DecoderT[F, S, A, O]
  )(eq: F[A]): IsEq[F[A]] =
    (for {
      encoded <- encoder.write(path, settings, a)
      finalized <- fin.run(path, settings, encoded)
      prepared <- prep.run(path, settings, finalized)
      decoded <- decoder.read(path, settings, None, prepared)
    } yield decoded) <-> eq

  def encodeFinalizePrepareDecode[A](
    a: A,
    path: List[String]
  )(implicit encoder: EncoderT[F, S, A, E], decoder: DecoderT[F, S, A, O]): IsEq[F[A]] =
    test(path, a)(encoder, finalise, prepare, decoder)(F.pure(a))

  def encodeFinalizePrepareDecodeFail[A](a: A, path: List[String])(
    implicit encoder: EncoderT[F, S, A, E]
  ): IsEq[F[A]] = {
    implicit val decoder: DecoderT[F, S, A, O] = new DecoderT[F, S, A, O] {
      override def read(path: List[String], settings: S, default: Option[A], input: O): F[A] =
        errors.validationFailure("fail")
    }

    test(path, a)(encoder, finalise, prepare, decoder)(errors.validationFailure("fail"))
  }

  def encodeFailFinalizePrepareDecode[A](a: A, path: List[String])(
    implicit decoder: DecoderT[F, S, A, O]
  ): IsEq[F[A]] = {
    implicit val encoder: EncoderT[F, S, A, E] = new EncoderT[F, S, A, E] {
      override def write(path: List[String], settings: S, in: A): F[E] = errors.validationFailure("fail")
    }

    test(path, a)(encoder, finalise, prepare, decoder)(errors.validationFailure("fail"))
  }

  def encodeFinalizeFailPrepareDecode[A](
    a: A,
    path: List[String]
  )(implicit encoder: EncoderT[F, S, A, E], decoder: DecoderT[F, S, A, O]): IsEq[F[A]] = {
    val fin = new Transform[F, S, E, D] {
      override def run(namespace: List[String], settings: S, input: E): F[D] = errors.validationFailure("fail")
    }

    test(path, a)(encoder, fin, prepare, decoder)(errors.validationFailure("fail"))
  }

  def encodeFinalizePrepareFailDecode[A](
    a: A,
    path: List[String]
  )(implicit encoder: EncoderT[F, S, A, E], decoder: DecoderT[F, S, A, O]): IsEq[F[A]] = {
    val prep = new Transform[F, S, D, O] {
      override def run(namespace: List[String], settings: S, input: D): F[O] = errors.validationFailure("fail")
    }

    test(path, a)(encoder, finalise, prep, decoder)(errors.validationFailure("fail"))
  }

  def encodeDecodeWithPartiallyApplied[A](
    a: A,
    path: List[String]
  )(implicit encoder: EncoderT[F, S, A, E], decoder: DecoderT[F, S, A, O]): IsEq[F[A]] = {
    val encode = new EncodePartiallyAppliedWithDefaultSettings[F, S, E, D] {
      override protected def defaultSettings: S = settings
    }
    val decode = new DecodePartiallyAppliedWithDefaultSettings[F, A, S, O, D] {
      override protected def defaultSettings: S = settings
    }

    (for {
      encoded <- encode(path, a)
      loadInput = new LoadInput[F, D] { override def load: F[D] = F.pure(encoded) }
      decoded <- decode(path)(decoder, F, loadInput, prepare)
    } yield decoded) <-> F.pure(a)
  }

  def combinedEncodeDecodeWithPartiallyApplied[A](
    a: A,
    path: List[String]
  )(implicit encoder: EncoderT[F, S, A, E], decoder: DecoderT[F, S, A, O]): IsEq[F[A]] = {
    val encode = new EncodePartiallyAppliedWithDefaultSettings[F, S, E, D] {
      override protected def defaultSettings: S = settings
    }
    val decode = new DecodePartiallyAppliedWithDefaultSettings[F, A, S, O, D] {
      override protected def defaultSettings: S = settings
    }

    (for {
      encoded <- encode.combine(encode)(path, a)
      loadInput = new LoadInput[F, (D, D)] {
        override def load: F[(D, D)] = encoded match {
          case Ior.Both(d0, d1) => F.pure((d0, d1))
          case Ior.Right(_) => errors.validationFailure("Ior is right")
          case Ior.Left(_) => errors.validationFailure("Ior is left")
        }
      }
      decoded <- decode.combine(decode)(path)(
        DecoderT[F, (S, S), A, (O, O)],
        F,
        loadInput,
        Transform[F, (S, S), (D, D), (O, O)]
      )
    } yield decoded) <-> F.pure(a)
  }

  def combinedEncodeDecodeWithPartiallyAppliedRightFail[A](
    a: A,
    path: List[String]
  )(implicit encoder: EncoderT[F, S, A, E], decoder: DecoderT[F, S, A, O]): IsEq[F[A]] = {
    implicit val failDecoder: DecoderT[F, S, A, Lazy[O]] = new DecoderT[F, S, A, Lazy[O]] {
      override def read(path: List[String], settings: S, default: Option[A], input: Lazy[O]): F[A] =
        errors.validationFailure("Fail")
    }

    implicit val transformLazy: Transform[F, S, D, Lazy[O]] = Transform.by[F, S, D, O, Lazy[O]](Lazy(_))

    val encode = new EncodePartiallyAppliedWithDefaultSettings[F, S, E, D] {
      override protected def defaultSettings: S = settings
    }

    val decode = new DecodePartiallyAppliedWithDefaultSettings[F, A, S, O, D] {
      override protected def defaultSettings: S = settings
    }

    val decodeLazy = new DecodePartiallyAppliedWithDefaultSettings[F, A, S, Lazy[O], D] {
      override protected def defaultSettings: S = settings
    }

    (for {
      encoded <- encode.combine(encode)(path, a)
      loadInput = new LoadInput[F, (D, D)] {
        override def load: F[(D, D)] = encoded match {
          case Ior.Both(d0, d1) => F.pure((d0, d1))
          case Ior.Right(_) => errors.validationFailure("Ior is right")
          case Ior.Left(_) => errors.validationFailure("Ior is left")
        }
      }
      decoded <- decodeLazy.combine(decode)(path)(
        DecoderT[F, (S, S), A, (Lazy[O], O)],
        F,
        loadInput,
        Transform[F, (S, S), (D, D), (Lazy[O], O)]
      )
    } yield decoded) <-> F.pure(a)
  }
}

object EncoderDecoderLaws {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](
    s: S
  )(implicit fin: Transform[F, S, E, D], prep: Transform[F, S, D, O]): EncoderDecoderLaws[F, S, E, D, O] =
    new EncoderDecoderLaws[F, S, E, D, O] {
      override def F: Monad[F] = Monad[F]
      override def settings: S = s
      override def finalise: Transform[F, S, E, D] = fin
      override def prepare: Transform[F, S, D, O] = prep
      override def monoid: Monoid[E] = Monoid[E]
      override def outMonoid: Monoid[O] = Monoid[O]
      override def errors: ExtruderErrors[F] = ExtruderErrors[F]
    }
}
