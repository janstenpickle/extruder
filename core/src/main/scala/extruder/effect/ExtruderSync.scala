package extruder.effect

import cats.data.Validated.{Invalid, Valid}
import cats.effect.Sync
import cats.syntax.validated._
import cats.{Applicative, Apply, Monad, MonadError}
import extruder.core.{Missing, Validation, ValidationException, ValidationFailure}
import extruder.data.ValidationT
import extruder.instances.ValidationInstances

import scala.util.Either

trait ExtruderSync[F[_]] extends Sync[F] with ExtruderMonadError[F]

trait LowPrioritySyncInstances {
  implicit def validationTSync[F[_]: Sync]: ExtruderSync[ValidationT[F, ?]] = new ValidationTSync[F] {
    override protected def FF: Sync[F] = Sync[F]
  }

  trait ValidationTSync[F[_]] extends ExtruderSync[ValidationT[F, ?]] {
    protected def FF: Sync[F]

    override def missing[A](message: String): ValidationT[F, A] =
      ValidationT(FF.pure(Missing(message).invalidNel))

    override def validationFailure[A](message: String): ValidationT[F, A] =
      ValidationT(FF.pure(ValidationFailure(message).invalidNel))

    override def validationException[A](message: String, ex: Throwable): ValidationT[F, A] =
      ValidationT(FF.pure(ValidationException(message, ex).invalidNel))

    override def raiseError[A](e: Throwable): ValidationT[F, A] = ValidationT(FF.raiseError(e))

    override def pure[A](x: A): ValidationT[F, A] = ValidationT(FF.pure(x.validNel))

    override def suspend[A](thunk: => ValidationT[F, A]): ValidationT[F, A] = ValidationT(FF.suspend(thunk.value))

    override def flatMap[A, B](fa: ValidationT[F, A])(f: A => ValidationT[F, B]): ValidationT[F, B] =
      ValidationT[F, B](FF.flatMap(fa.value) {
        case inv @ Invalid(_) => FF.pure(inv)
        case Valid(a) => f(a).value
      })

    override def tailRecM[A, B](a: A)(f: A => ValidationT[F, Either[A, B]]): ValidationT[F, B] = ValidationT[F, B](
      FF.tailRecM(a)(
        a0 =>
          FF.map(f(a0).value) {
            case Invalid(e) => Right(Invalid(e))
            case Valid(Left(a1)) => Left(a1)
            case Valid(Right(b)) => Right(Valid(b))
        }
      )
    )

    override def handleErrorWith[A](fa: ValidationT[F, A])(f: Throwable => ValidationT[F, A]) =
      ValidationT(FF.handleErrorWith(fa.value)(f.andThen(_.value)))

    override def ap2[A, B, Z](
      ff: ValidationT[F, (A, B) => Z]
    )(fa: ValidationT[F, A], fb: ValidationT[F, B]): ValidationT[F, Z] =
      ValidationT(
        FF.flatMap(fa.value)(
          fa0 => FF.flatMap(fb.value)(fb0 => FF.map(ff.value)(ff0 => Apply[Validation].ap2(ff0)(fa0, fb0)))
        )
      )
  }
}

trait SyncInstances {
  abstract class FromApplicative[F[_]](implicit F: Applicative[F]) extends ExtruderSync[F] {
    def suspend[A](thunk: => F[A]): F[A] = thunk
    def pure[A](x: A): F[A] = F.pure(x)
  }

  abstract class FromMonad[F[_]](implicit F: Monad[F]) extends FromApplicative[F] {
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
  }

  class FromMonadError[F[_]](implicit F: MonadError[F, Throwable]) extends FromMonad[F] {
    override def missing[A](message: String): F[A] = raiseError(new NoSuchElementException(message))
    override def validationFailure[A](message: String): F[A] = raiseError(new RuntimeException(message))
    override def validationException[A](message: String, ex: Throwable): F[A] = raiseError(ex)
    override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
    override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)
  }

  implicit def fromSync[F[_]](implicit F: Sync[F]): ExtruderSync[F] =
    new FromMonadError[F]()(F) {
      override def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
      override def delay[A](thunk: => A): F[A] = F.delay(thunk)
    }
}

object ExtruderSync extends SyncInstances with LowPrioritySyncInstances with ValidationInstances {
  def apply[F[_]](implicit sync: ExtruderSync[F]): ExtruderSync[F] = sync
}
