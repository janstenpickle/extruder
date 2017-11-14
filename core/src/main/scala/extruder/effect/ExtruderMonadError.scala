package extruder.effect

import cats.data.Validated.{Invalid, Valid}
import cats.{Applicative, Apply, Monad, MonadError}
import extruder.core.{Missing, Validation, ValidationException, ValidationFailure}
import extruder.data.ValidationT
import extruder.instances.ValidationInstances

import scala.util.Either

import cats.syntax.validated._

trait ExtruderMonadError[F[_]] extends MonadError[F, Throwable] {
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]
}

trait LowPriorityMonadErrorInstances {
  implicit def validationTMonadError[F[_]](
    implicit ev0: MonadError[F, Throwable]
  ): ExtruderMonadError[ValidationT[F, ?]] =
    new ValidationTMonadError[F] {
      override protected def FFF: MonadError[F, Throwable] = MonadError[F, Throwable]
    }

  trait ValidationTMonadError[F[_]] extends ExtruderMonadError[ValidationT[F, ?]] {
    protected def FFF: MonadError[F, Throwable]

    override def missing[A](message: String): ValidationT[F, A] =
      ValidationT(FFF.pure(Missing(message).invalidNel))

    override def validationFailure[A](message: String): ValidationT[F, A] =
      ValidationT(FFF.pure(ValidationFailure(message).invalidNel))

    override def validationException[A](message: String, ex: Throwable): ValidationT[F, A] =
      ValidationT(FFF.pure(ValidationException(message, ex).invalidNel))

    override def raiseError[A](e: Throwable): ValidationT[F, A] = ValidationT(FFF.raiseError(e))

    override def pure[A](x: A): ValidationT[F, A] = ValidationT(FFF.pure(x.validNel))

    override def flatMap[A, B](fa: ValidationT[F, A])(f: A => ValidationT[F, B]): ValidationT[F, B] =
      ValidationT[F, B](FFF.flatMap(fa.value) {
        case inv @ Invalid(_) => FFF.pure(inv)
        case Valid(a) => f(a).value
      })

    override def tailRecM[A, B](a: A)(f: A => ValidationT[F, Either[A, B]]): ValidationT[F, B] = ValidationT[F, B](
      FFF.tailRecM(a)(
        a0 =>
          FFF.map(f(a0).value) {
            case Invalid(e) => Right(Invalid(e))
            case Valid(Left(a1)) => Left(a1)
            case Valid(Right(b)) => Right(Valid(b))
        }
      )
    )

    override def handleErrorWith[A](fa: ValidationT[F, A])(f: Throwable => ValidationT[F, A]) =
      ValidationT(FFF.handleErrorWith(fa.value)(f.andThen(_.value)))

    override def ap2[A, B, Z](
      ff: ValidationT[F, (A, B) => Z]
    )(fa: ValidationT[F, A], fb: ValidationT[F, B]): ValidationT[F, Z] =
      ValidationT(
        FFF.flatMap(fa.value)(
          fa0 => FFF.flatMap(fb.value)(fb0 => FFF.map(ff.value)(ff0 => Apply[Validation].ap2(ff0)(fa0, fb0)))
        )
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

  implicit def fromMonadError[F[_]](implicit F: MonadError[F, Throwable]): ExtruderMonadError[F] =
    new ExtruderMonadError[F] {
      override def missing[A](message: String): F[A] = raiseError(new NoSuchElementException(message))
      override def validationFailure[A](message: String): F[A] = raiseError(new RuntimeException(message))
      override def validationException[A](message: String, ex: Throwable): F[A] = raiseError(ex)
      override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
      override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)

      override def pure[A](x: A) = F.pure(x)

      override def flatMap[A, B](fa: F[A])(f: A => F[B]) = F.flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]) = F.tailRecM(a)(f)
    }

  //implicit def fromExtruderAsync[F[_]](implicit F: ExtruderAsync[F]): ExtruderMonadError[F] = fromMonadError(F)
}

object ExtruderMonadError
    extends ExtruderMonadErrorInstances
    with LowPriorityMonadErrorInstances
    with ValidationInstances {
  def apply[F[_]](implicit monadError: ExtruderMonadError[F]): ExtruderMonadError[F] = monadError
}
