package extruder.effect

import cats.data.EitherT
import cats.effect._
import cats.{Applicative, Monad}
import extruder.core.ValidationErrors
import extruder.instances.EitherInstances
import shapeless.LowPriority

trait ExtruderAsync[F[_]] extends ExtruderSync[F] with Async[F]

trait LowPriorityAsyncInstances {
  implicit def eitherTAsync[F[_]: Async]: ExtruderAsync[EitherT[F, ValidationErrors, ?]] =
    new EitherTAsync[F] {
      override protected def F: Monad[F] = Monad[F]

      override protected def FE: Async[EitherT[F, ValidationErrors, ?]] = Async.catsEitherTAsync
    }

  trait EitherTAsync[F[_]] extends ExtruderSync.EitherTSync[F] with ExtruderAsync[EitherT[F, ValidationErrors, ?]] {
    protected def FE: Async[EitherT[F, ValidationErrors, ?]]
    override protected def FFE: Sync[EitherT[F, ValidationErrors, ?]] = FE

    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): EitherT[F, ValidationErrors, A] =
      FE.async(k)

    override def liftIO[A](ioa: IO[A]): EitherT[F, ValidationErrors, A] = FE.liftIO(ioa)
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

object ExtruderAsync extends AsyncInstances with LowPriorityAsyncInstances with EitherInstances {
  def apply[F[_]](implicit async: ExtruderAsync[F]): ExtruderAsync[F] = async
}
