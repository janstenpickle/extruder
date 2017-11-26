package extruder.effect

import cats.data.EitherT
import cats.effect.Sync
import cats.{Applicative, Monad, MonadError}
import extruder.core.ValidationErrors
import extruder.instances.EitherInstances
import shapeless.LowPriority

trait ExtruderSync[F[_]] extends Sync[F] with ExtruderMonadError[F]

trait LowPrioritySyncInstances {
  implicit def eitherTSync[F[_]: Sync]: ExtruderSync[EitherT[F, ValidationErrors, ?]] = new EitherTSync[F] {
    override protected def F: Monad[F] = Monad[F]

    override protected def FFE: Sync[EitherT[F, ValidationErrors, ?]] = Sync.catsEitherTSync
  }

  trait EitherTSync[F[_]]
      extends ExtruderMonadError.EitherTMonadError[F]
      with ExtruderSync[EitherT[F, ValidationErrors, ?]] {
    protected def FFE: Sync[EitherT[F, ValidationErrors, ?]]

    override protected def FFFE: MonadError[EitherT[F, ValidationErrors, ?], Throwable] =
      FFE

    override def suspend[A](thunk: => EitherT[F, ValidationErrors, A]): EitherT[F, ValidationErrors, A] =
      FFE.suspend(thunk)
  }
}

trait SyncInstances {
  class FromSync[F[_]](implicit F: Sync[F]) extends ExtruderMonadError.FromMonadError[F]()(F) with ExtruderSync[F] {
    override def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
    override def delay[A](thunk: => A): F[A] = F.delay(thunk)
  }

  implicit def fromSync[F[_]](implicit F: Sync[F], lp: LowPriority): ExtruderSync[F] =
    new FromSync[F]()(F)

  implicit def eitherTFromSync[F[_]](
    implicit F: Sync[EitherT[F, Throwable, ?]],
    FF: Applicative[F]
  ): ExtruderSync[EitherT[F, Throwable, ?]] = new FromSync[EitherT[F, Throwable, ?]]()(F) {
    override def validationException[A](message: String, ex: Throwable): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(ex)))
    override def missing[A](message: String): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(new NoSuchElementException(message))))
    override def validationFailure[A](message: String): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(new RuntimeException(message))))
  }
}

object ExtruderSync extends EitherInstances with SyncInstances with LowPrioritySyncInstances {
  def apply[F[_]](implicit sync: ExtruderSync[F]): ExtruderSync[F] = sync
}
