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
