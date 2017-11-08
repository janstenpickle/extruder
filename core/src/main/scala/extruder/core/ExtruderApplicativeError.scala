package extruder.core

import cats.data.NonEmptyList
import cats.effect.{Async, Effect, IO}
import cats.{ApplicativeError, FlatMap, MonadError, Traverse}
import extruder.instances.{EitherInstances, FutureInstances, IOInstances, ValidationInstances}

import scala.util.{Either, Failure, Success, Try}

trait ExtruderApplicativeError[F[_], E] {

  def catchNonFatal[A](value: => A): F[A] = Try(value) match {
    case Failure(ex) => validationException(ex.getMessage, ex)
    case Success(v) => pure(v)
  }

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]

  def handleErrorWith[A](fa: F[A])(f: (E) => F[A]): F[A]
  def raiseError[A](e: E): F[A]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] =
    map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }

  def sequence[G[_], A](as: G[F[A]])(implicit G: Traverse[G]): F[G[A]]
  //def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = FM.tailRecM(a)(f)

  def attempt[A](fa: F[A]): F[Either[E, A]] = handleErrorWith(map(fa)(Right(_): Either[E, A]))(e => pure(Left(e)))

  def attemptIO[A](a: IO[F[A]])(implicit hints: Hints): F[A] =
    flatMap(catchNonFatal(a.unsafeRunTimed(hints.ioTimeout)))(
      _.fold[F[A]](validationFailure("Failed to evaluate IO, timed out"))(identity)
    )
}

object ExtruderApplicativeError
    extends ValidationInstances
    with FutureInstances
    with EitherInstances
    with IOInstances {
  def appendThrowables[A, B]: (EitherThrowable[(A => B)], EitherThrowable[A]) => EitherThrowable[B] = {
    case (Right(f), Right(a)) => Right(f(a))
    case (Left(fe), Left(ae)) =>
      Left {
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

  trait WithMonadError[F[_], E] extends ExtruderApplicativeError[F, E] {
    def ME: MonadError[F, E]

    override def raiseError[A](e: E): F[A] = ME.raiseError(e)
    override def handleErrorWith[A](fa: F[A])(f: (E) => F[A]): F[A] = ME.handleErrorWith(fa)(f)
    override def pure[A](x: A): F[A] = ME.pure(x)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = ME.map(fa)(f)
    override def sequence[G[_], A](as: G[F[A]])(implicit G: Traverse[G]): F[G[A]] = ME.sequence(as)

    override def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] = ME.ap(ff)(fa)

    override def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = ME.flatMap(fa)(f)
  }

  implicit def fromMonadError[F[_]](implicit _ME: MonadError[F, Throwable]): ExtruderApplicativeError[F, Throwable] =
    new WithMonadError[F, Throwable] {
      override def ME: MonadError[F, Throwable] = _ME

      override def missing[A](message: String): F[A] = ME.raiseError(new NoSuchElementException(message))

      override def validationFailure[A](message: String): F[A] = ME.raiseError(new RuntimeException(message))

      override def validationException[A](message: String, ex: Throwable): F[A] = ME.raiseError(ex)
    }

//  class FromMonadError[M[_]](implicit override val ME: MonadError[M, Throwable]) extends WithMonadError[M, Throwable] {
//    override def missing[A](message: String): M[A] = ME.raiseError(new NoSuchElementException(message))
//    override def validationFailure[A](message: String): M[A] = ME.raiseError(new RuntimeException(message))
//    override def validationException[A](message: String, ex: Throwable): M[A] = ME.raiseError(ex)
//    override def ap[A, B](ff: M[(A) => B])(fa: M[A]): M[B] = ME.ap(ff)(fa)
//  }

  abstract class FromMonadErrorAccumulatesErrors[M[_]](implicit override val ME: MonadError[M, ValidationErrors])
      extends WithMonadError[M, ValidationErrors]
      with AccumulatesErrors[M]

  def apply[M[_], E](implicit AE: ExtruderApplicativeError[M, E]): ExtruderApplicativeError[M, E] = AE
}
