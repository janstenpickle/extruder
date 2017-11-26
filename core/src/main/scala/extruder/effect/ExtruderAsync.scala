package extruder.effect

import cats.effect._
import cats.syntax.validated._
import extruder.data.ValidationT
import extruder.instances.{EitherInstances, ValidationInstances}

import scala.util.Either

trait ExtruderAsync[F[_]] extends Async[F] {
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]
}

abstract class LowPriorityAsyncInstances {
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

abstract class AsyncInstances extends LowPriorityAsyncInstances {
  abstract class FromSync[F[_]](implicit F: Sync[F]) extends ExtruderAsync[F] {
    override def pure[A](x: A): F[A] = F.pure(x)
    override def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
    override def delay[A](thunk: => A): F[A] = F.delay(thunk)
    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
    override def missing[A](message: String): F[A] = raiseError(new NoSuchElementException(message))
    override def validationFailure[A](message: String): F[A] = raiseError(new RuntimeException(message))
    override def validationException[A](message: String, ex: Throwable): F[A] = raiseError(ex)
    override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
    override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)
  }

  implicit def fromAsync[F[_]](implicit F: Async[F]): ExtruderAsync[F] =
    new FromSync[F]()(F) {
      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] = F.async(k)
      override def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
      override def delay[A](thunk: => A): F[A] = F.delay(thunk)
      override def liftIO[A](ioa: IO[A]): F[A] = F.liftIO(ioa)
    }
}

object ExtruderAsync extends AsyncInstances with ValidationInstances with EitherInstances {
  def apply[F[_]](implicit async: ExtruderAsync[F]): ExtruderAsync[F] = async
}
