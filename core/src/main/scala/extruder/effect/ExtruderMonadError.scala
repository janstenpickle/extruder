package extruder.effect

import cats.data.{EitherT, NonEmptyList}
import cats.{Applicative, Monad, MonadError}
import extruder.core._
import extruder.instances.EitherInstances
import shapeless.LowPriority

trait ExtruderMonadError[F[_]] extends MonadError[F, Throwable] {
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]
}

trait LowPriorityMonadErrorInstances { self: EitherInstances =>
  implicit def eitherTMonadError[F[_]](
    implicit ev0: MonadError[F, Throwable]
  ): ExtruderMonadError[EitherT[F, ValidationErrors, ?]] = new EitherTMonadError[F] {
    override protected def F: Monad[F] = Monad[F]

    override protected def FFFE: MonadError[EitherT[F, ValidationErrors, ?], Throwable] =
      EitherT.catsDataMonadErrorFForEitherT
  }

  trait EitherTMonadError[F[_]] extends ExtruderMonadError[EitherT[F, ValidationErrors, ?]] {
    protected def F: Monad[F]
    protected def FFFE: MonadError[EitherT[F, ValidationErrors, ?], Throwable]

    override def missing[A](message: String): EitherT[F, ValidationErrors, A] =
      EitherT.leftT[F, A].apply[ValidationErrors](NonEmptyList.of(Missing(message)))(F)

    override def validationFailure[A](message: String): EitherT[F, ValidationErrors, A] =
      EitherT.leftT[F, A].apply[ValidationErrors](NonEmptyList.of(ValidationFailure(message)))(F)

    override def validationException[A](message: String, ex: Throwable): EitherT[F, ValidationErrors, A] =
      EitherT.leftT[F, A].apply[ValidationErrors](NonEmptyList.of(ValidationException(message, ex)))(F)

    override def flatMap[A, B](
      fa: EitherT[F, ValidationErrors, A]
    )(f: A => EitherT[F, ValidationErrors, B]): EitherT[F, ValidationErrors, B] =
      FFFE.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(
      f: A => EitherT[F, ValidationErrors, Either[A, B]]
    ): EitherT[F, ValidationErrors, B] = FFFE.tailRecM(a)(f)

    override def raiseError[A](e: Throwable): EitherT[F, ValidationErrors, A] = FFFE.raiseError(e)

    override def handleErrorWith[A](
      fa: EitherT[F, ValidationErrors, A]
    )(f: Throwable => EitherT[F, ValidationErrors, A]): EitherT[F, ValidationErrors, A] =
      FFFE.handleErrorWith(fa)(f)

    override def pure[A](x: A): EitherT[F, ValidationErrors, A] = FFFE.pure(x)

    override def ap[A, B](
      ff: EitherT[F, ValidationErrors, A => B]
    )(fa: EitherT[F, ValidationErrors, A]): EitherT[F, ValidationErrors, B] =
      EitherT(F.flatMap(fa.value)(fa0 => F.map(ff.value)(ff0 => eitherApply.ap(ff0)(fa0))))

    override def ap2[A, B, Z](
      ff: EitherT[F, ValidationErrors, (A, B) => Z]
    )(fa: EitherT[F, ValidationErrors, A], fb: EitherT[F, ValidationErrors, B]): EitherT[F, ValidationErrors, Z] =
      EitherT(
        F.flatMap(fa.value)(fa0 => F.flatMap(fb.value)(fb0 => F.map(ff.value)(ff0 => eitherApply.ap2(ff0)(fa0, fb0))))
      )
  }
}

trait ExtruderMonadErrorInstances {
  abstract class FromApplicative[F[_]](implicit F: Applicative[F]) extends ExtruderMonadError[F] {
    override def pure[A](x: A): F[A] = F.pure(x)
  }

  abstract class FromMonad[F[_]](implicit F: Monad[F]) extends FromApplicative[F] {
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
  }

  class FromMonadError[F[_]](implicit F: MonadError[F, Throwable]) extends FromMonad[F]()(F) {
    override def missing[A](message: String): F[A] = raiseError(new NoSuchElementException(message))
    override def validationFailure[A](message: String): F[A] = raiseError(new RuntimeException(message))
    override def validationException[A](message: String, ex: Throwable): F[A] = raiseError(ex)
    override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
    override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)
  }

  implicit def fromMonadError[F[_]](implicit F: MonadError[F, Throwable], lp: LowPriority): ExtruderMonadError[F] =
    new FromMonadError[F]()(F)

  implicit def eitherTFromMonadError[F[_]](
    implicit F: MonadError[EitherT[F, Throwable, ?], Throwable],
    FF: Applicative[F]
  ): ExtruderMonadError[EitherT[F, Throwable, ?]] = new FromMonadError[EitherT[F, Throwable, ?]]()(F) {
    override def validationException[A](message: String, ex: Throwable): EitherT[F, Throwable, A] =
      EitherT.leftT(ex)
    override def missing[A](message: String): EitherT[F, Throwable, A] =
      EitherT.leftT(new NoSuchElementException(message))
    override def validationFailure[A](message: String): EitherT[F, Throwable, A] =
      EitherT.leftT(new RuntimeException(message))
  }
}

object ExtruderMonadError extends EitherInstances with ExtruderMonadErrorInstances with LowPriorityMonadErrorInstances {
  def apply[F[_]](implicit monadError: ExtruderMonadError[F]): ExtruderMonadError[F] = monadError
}
