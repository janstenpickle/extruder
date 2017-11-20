package extruder.effect

import cats.Applicative
import cats.data.EitherT
import cats.effect._
import cats.syntax.validated._
import extruder.data.ValidationT
import extruder.instances.{EitherInstances, ValidationInstances}
import shapeless.LowPriority

import scala.util.Either

trait ExtruderAsync[F[_]] extends ExtruderSync[F] with Async[F]

trait LowPriorityAsyncInstances {
  implicit def validationTAsync[F[_]: Async]: ExtruderAsync[ValidationT[F, ?]] =
    new ValidationTAsync[F] {
      protected def F: Async[F] = Async[F]
    }

  trait ValidationTAsync[F[_]] extends ExtruderSync.ValidationTSync[F] with ExtruderAsync[ValidationT[F, ?]] {
    protected implicit def F: Async[F]
    override protected implicit def FF: Sync[F] = F

    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ValidationT[F, A] =
      ValidationT(F.map(F.async(k))(_.validNel))

    override def liftIO[A](ioa: IO[A]): ValidationT[F, A] =
      ValidationT(F.map(F.liftIO(ioa))(_.validNel))
  }
}

trait AsyncInstances {
  class FromAsync[F[_]](implicit F: Async[F]) extends ExtruderSync.FromSync[F]()(F) with ExtruderAsync[F] {
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] = F.async(k)
    override def liftIO[A](ioa: IO[A]): F[A] = F.liftIO(ioa)
  }

  implicit def fromAsync[F[_]](implicit F: Async[F], lp: LowPriority): ExtruderAsync[F] =
    new FromAsync[F]()(F)

  implicit def eitherTFromAsync[F[_]](
    implicit F: Async[EitherT[F, Throwable, ?]],
    FF: Applicative[F]
  ): ExtruderAsync[EitherT[F, Throwable, ?]] = new FromAsync[EitherT[F, Throwable, ?]]()(F) {
    override def validationException[A](message: String, ex: Throwable): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(ex)))
    override def missing[A](message: String): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(new NoSuchElementException(message))))
    override def validationFailure[A](message: String): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(new RuntimeException(message))))
  }
}

object ExtruderAsync
    extends AsyncInstances
    with LowPriorityAsyncInstances
    with ValidationInstances
    with EitherInstances {
  def apply[F[_]](implicit async: ExtruderAsync[F]): ExtruderAsync[F] = async
}
