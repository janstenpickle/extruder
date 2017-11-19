package extruder.effect

import cats.data.EitherT
import cats.effect.Sync
import cats.{Applicative, Monad, MonadError}
import extruder.data.ValidationT
import extruder.instances.ValidationInstances
import shapeless.LowPriority

import scala.util.Either

trait ExtruderSync[F[_]] extends Sync[F] with ExtruderMonadError[F]

trait LowPrioritySyncInstances {
  implicit def validationTSync[F[_]: Sync]: ExtruderSync[ValidationT[F, ?]] = new ValidationTSync[F] {
    override protected def FF: Sync[F] = Sync[F]
  }

  trait ValidationTSync[F[_]]
      extends ExtruderMonadError.ValidationTMonadError[F]
      with ExtruderSync[ValidationT[F, ?]] {
    protected def FF: Sync[F]

    override protected def FFF: MonadError[F, Throwable] = FF

    override def suspend[A](thunk: => ValidationT[F, A]): ValidationT[F, A] = ValidationT(FF.suspend(thunk.value))
  }
}

trait SyncInstances {
  abstract class FromApplicative[F[_]](implicit F: Applicative[F]) extends ExtruderSync[F] {
    def pure[A](x: A): F[A] = F.pure(x)
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

  class FromSync[F[_]](implicit F: Sync[F]) extends FromMonadError[F]()(F) {
    override def suspend[A](thunk: => F[A]): F[A] = F.suspend(thunk)
    override def delay[A](thunk: => A): F[A] = F.delay(thunk)
  }

  implicit def fromSync[F[_]](implicit F: Sync[F], lp: LowPriority): ExtruderSync[F] =
    new FromSync[F]()(F)

  implicit def eitherTFromSync[F[_]](
    implicit F: Sync[EitherT[F, Throwable, ?]],
    FF: Applicative[F]
  ): ExtruderMonadError[EitherT[F, Throwable, ?]] = new FromSync[EitherT[F, Throwable, ?]]()(F) {
    override def validationException[A](message: String, ex: Throwable): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(ex)))
    override def missing[A](message: String): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(new NoSuchElementException(message))))
    override def validationFailure[A](message: String): EitherT[F, Throwable, A] =
      EitherT(FF.pure(Left(new RuntimeException(message))))
  }
}

object ExtruderSync extends ValidationInstances with SyncInstances with LowPrioritySyncInstances {
  def apply[F[_]](implicit sync: ExtruderSync[F]): ExtruderSync[F] = sync
}
