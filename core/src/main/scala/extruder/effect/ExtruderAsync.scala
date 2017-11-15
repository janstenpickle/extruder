package extruder.effect

import cats.{Applicative, Monad, MonadError}
import cats.effect._
import cats.syntax.validated._
import extruder.data.ValidationT
import extruder.instances.{EitherInstances, ValidationInstances}

import scala.util.Either

trait ExtruderAsync[F[_]] extends ExtruderSync[F] with Async[F]

trait LowPriorityAsyncInstances {
  implicit def validationTAsync[F[_]: Async]: ExtruderAsync[ValidationT[F, ?]] =
    new ValidationTAsync[F] {
      protected def F: Async[F] = Async[F]
    }

  trait ValidationTAsync[F[_]] extends ExtruderSync.ValidationTSync[F] with ExtruderAsync[ValidationT[F, ?]] {
    protected def F: Async[F]
    override protected def FF: Sync[F] = F

    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ValidationT[F, A] =
      ValidationT(F.map(F.async(k))(_.validNel))

    override def liftIO[A](ioa: IO[A]): ValidationT[F, A] =
      ValidationT(F.map(F.liftIO(ioa))(_.validNel))
  }
}

trait AsyncInstances {
  abstract class FromApplicative[F[_]](implicit F: Applicative[F]) extends ExtruderAsync[F] {
    override def pure[A](x: A): F[A] = F.pure(x)
  }

  abstract class FromMonad[F[_]](implicit F: Monad[F]) extends FromApplicative[F] {
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
  }

  abstract class FromMonadError[F[_]](implicit F: MonadError[F, Throwable]) extends FromMonad[F] {
    override def missing[A](message: String): F[A] = raiseError(new NoSuchElementException(message))
    override def validationFailure[A](message: String): F[A] = raiseError(new RuntimeException(message))
    override def validationException[A](message: String, ex: Throwable): F[A] = raiseError(ex)
    override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
    override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)
  }

  abstract class FromSync[F[_]](implicit F: Sync[F]) extends FromMonadError[F] {
    override def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
    override def delay[A](thunk: => A): F[A] = F.delay(thunk)
  }

  implicit def fromAsync[F[_]](implicit F: Async[F]): ExtruderAsync[F] =
    new FromSync[F]()(F) {
      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] = F.async(k)
      override def liftIO[A](ioa: IO[A]): F[A] = F.liftIO(ioa)
    }
}

object ExtruderAsync
    extends AsyncInstances
    with LowPrioritySyncInstances
    with ValidationInstances
    with EitherInstances {
  def apply[F[_]](implicit async: ExtruderAsync[F]): ExtruderAsync[F] = async
}
