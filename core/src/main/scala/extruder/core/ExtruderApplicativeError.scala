package extruder.core

import cats.data.NonEmptyList
import cats.effect.IO
import cats.{ApplicativeError, FlatMap, MonadError}
import extruder.instances.{ConfigValidationInstances, EitherInstances, FutureInstances, IOInstances}

import scala.util.{Failure, Success, Try}

abstract class ExtruderApplicativeError[F[_], E](implicit FM: FlatMap[F]) extends ApplicativeError[F, E] {
  def catchNonFatal[A](value: => A): F[A] = Try(value) match {
    case Failure(ex) => validationException(ex.getMessage, ex)
    case Success(v) => pure(v)
  }
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = FM.flatMap(fa)(f)
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]

  def attemptIO[A](a: IO[F[A]])(implicit utils: Hints): F[A] =
    flatMap(catchNonFatal(a.unsafeRunTimed(utils.ioTimeout)))(
      _.fold[F[A]](validationFailure("Failed to evaluate IO, timed out"))(identity)
    )
}

object ExtruderApplicativeError extends ConfigValidationInstances with FutureInstances with EitherInstances with IOInstances {
  def appendThrowables[A, B]: (EitherThrowable[(A => B)], EitherThrowable[A]) => EitherThrowable[B] = {
    case (Right(f), Right(a)) => Right(f(a))
    case (Left(fe), Left(ae)) => Left {
      fe.addSuppressed(ae)
      fe
    }
    case (Left(e), Right(_)) => Left(e)
    case (Right(_), Left(e)) => Left(e)
  }

  trait AccumulatesErrors[M[_]] extends ExtruderApplicativeError[M, ValidationErrors] {
    override def missing[A](message: String): M[A] =
      raiseError[A](NonEmptyList.of(Missing(message)))
    override def validationFailure[A](message: String): M[A] =
      raiseError[A](NonEmptyList.of(ValidationFailure(message)))
    override def validationException[A](message: String, ex: Throwable): M[A] =
      raiseError[A](NonEmptyList.of(ValidationException(message, ex)))
  }

  trait WithMonadError[M[_], E] extends ExtruderApplicativeError[M, E] {
    def ME: MonadError[M, E]

    override def raiseError[A](e: E): M[A] = ME.raiseError(e)
    override def handleErrorWith[A](fa: M[A])(f: (E) => M[A]): M[A] = ME.handleErrorWith(fa)(f)
    override def pure[A](x: A): M[A] = ME.pure(x)
    override def map[A, B](fa: M[A])(f: A => B): M[B] = ME.map(fa)(f)
  }

  class FromMonadError[M[_]](implicit override val ME: MonadError[M, Throwable]) extends WithMonadError[M, Throwable] {
    override def missing[A](message: String): M[A] = ME.raiseError(new NoSuchElementException(message))
    override def validationFailure[A](message: String): M[A] = ME.raiseError(new RuntimeException(message))
    override def validationException[A](message: String, ex: Throwable): M[A] = ME.raiseError(ex)
    override def ap[A, B](ff: M[(A) => B])(fa: M[A]): M[B] = ME.ap(ff)(fa)
  }

  abstract class FromMonadErrorAccumulatesErrors[M[_]](implicit override val ME: MonadError[M, ValidationErrors])
    extends WithMonadError[M, ValidationErrors] with AccumulatesErrors[M]


  def apply[M[_], E](AE: ExtruderApplicativeError[M, E]): ExtruderApplicativeError[M, E] = AE
}
