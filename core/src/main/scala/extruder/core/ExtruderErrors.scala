package extruder.core

import cats.ApplicativeError
import shapeless.LowPriority

trait ExtruderErrors[F[_]] {
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]
  def validationException[A](ex: Throwable): F[A] = validationException(ex.getMessage, ex)
  def fallback[A](fa: F[A])(thunk: => F[A]): F[A]
}

object ExtruderErrors  {
  def apply[F[_]](implicit value: ExtruderErrors[F]): ExtruderErrors[F] = value

  implicit def applicativeError[F[_]](implicit F: ApplicativeError[F, Throwable], lp: LowPriority): ExtruderErrors[F] =
    new ExtruderErrors[F] {
      override def missing[A](message: String): F[A] = F.raiseError(new NoSuchElementException(message))
      override def validationFailure[A](message: String): F[A] = F.raiseError(new RuntimeException(message))
      override def validationException[A](message: String, ex: Throwable): F[A] = F.raiseError(ex)
      override def fallback[A](a: F[A])(thunk: => F[A]): F[A] = F.handleErrorWith(a)(_ => thunk)
    }
}
