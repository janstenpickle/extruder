package extruder.core

import cats.data.Validated.{Invalid, Valid}
import cats.effect.{Effect, IO}
import cats.syntax.validated._
import cats.{Applicative, Apply, Monad, MonadError}
import extruder.data.ValidationT
import extruder.instances.{EitherInstances, ValidationInstances}

import scala.util.{Either, Failure, Success, Try}

trait ExtruderEffect[F[_]] extends Effect[F] {
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]
}

trait LowPriorityEffectInstances {
  implicit def validationTEffect[F[_]: Effect]: ExtruderEffect[ValidationT[F, ?]] =
    new ExtruderEffect[ValidationT[F, ?]] {
      protected def F: Effect[F] = Effect[F]

      override def missing[A](message: String): ValidationT[F, A] =
        ValidationT(F.pure(Missing(message).invalidNel))

      override def validationFailure[A](message: String): ValidationT[F, A] =
        ValidationT(F.pure(ValidationFailure(message).invalidNel))

      override def validationException[A](message: String, ex: Throwable): ValidationT[F, A] =
        ValidationT(F.pure(ValidationException(message, ex).invalidNel))

      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ValidationT[F, A] =
        ValidationT[F, A](F.map(F.async(k))(_.valid))

      override def raiseError[A](e: Throwable): ValidationT[F, A] = ValidationT(F.raiseError(e))

      override def pure[A](x: A): ValidationT[F, A] = ValidationT(F.pure(x.validNel))

      override def runAsync[A](fa: ValidationT[F, A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
        F.runAsync(fa.value) {
          case Left(th) => cb(Left(th))
          case Right(Valid(a)) => cb(Right(a))
          case Right(Invalid(e)) => cb(Left(errorsToThrowable(e)))
        }

      override def suspend[A](thunk: => ValidationT[F, A]): ValidationT[F, A] = ValidationT(F.suspend(thunk.value))

      override def flatMap[A, B](fa: ValidationT[F, A])(f: A => ValidationT[F, B]): ValidationT[F, B] =
        ValidationT[F, B](F.flatMap(fa.value) {
          case inv @ Invalid(_) => F.pure(inv)
          case Valid(a) => f(a).value
        })

      override def tailRecM[A, B](a: A)(f: A => ValidationT[F, Either[A, B]]): ValidationT[F, B] = ValidationT[F, B](
        F.tailRecM(a)(
          a0 =>
            F.map(f(a0).value) {
              case Invalid(e) => Right(Invalid(e))
              case Valid(Left(a1)) => Left(a1)
              case Valid(Right(b)) => Right(Valid(b))
          }
        )
      )

      override def handleErrorWith[A](fa: ValidationT[F, A])(f: Throwable => ValidationT[F, A]) =
        ValidationT(F.handleErrorWith(fa.value)(f.andThen(_.value)))

      override def ap2[A, B, Z](
        ff: ValidationT[F, (A, B) => Z]
      )(fa: ValidationT[F, A], fb: ValidationT[F, B]): ValidationT[F, Z] =
        ValidationT(
          F.flatMap(fa.value)(
            fa0 => F.flatMap(fb.value)(fb0 => F.map(ff.value)(ff0 => Apply[Validation].ap2(ff0)(fa0, fb0)))
          )
        )
    }
}

trait EffectInstances {
  abstract class FromApplicative[F[_]](implicit F: Applicative[F]) extends ExtruderEffect[F] {
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] =
      Try(IO.async(k).unsafeRunSync()) match {
        case Failure(ex) => raiseError(ex)
        case Success(a) => F.pure(a)
      }

    override def runAsync[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
      IO(())

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

  implicit def fromEffect[F[_]: Effect](implicit F: Effect[F]): ExtruderEffect[F] =
    new FromMonadError[F]()(F) {
      override def runAsync[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] = F.runAsync(fa)(cb)
      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] = F.async(k)
      override def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
      override def delay[A](thunk: => A): F[A] = F.delay(thunk)
    }
}

object ExtruderEffect
    extends EffectInstances
    with ValidationInstances
    with EitherInstances
    with LowPriorityEffectInstances {
  def apply[F[_]](implicit effect: ExtruderEffect[F]): ExtruderEffect[F] = effect
}
