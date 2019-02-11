package extruder.laws

import cats.Applicative
import cats.laws._
import extruder.core.ExtruderErrors

import scala.util.Failure

trait ExtruderErrorsLaws[F[_]] {
  implicit def F: Applicative[F]
  implicit def errors: ExtruderErrors[F]

  def fallbackWithPure[A](a: A, fa: F[A]): IsEq[F[A]] =
    errors.fallback(F.pure(a))(fa) <-> F.pure(a)

  def validationException[A](ex: Throwable): IsEq[F[A]] =
    errors.validationException[A](ex.getMessage, ex) <-> errors.validationException[A](ex)

  def fromTry[A](ex: Throwable): IsEq[F[A]] =
    errors.fromTry[A](Failure(ex)) <-> errors.validationException[A](ex)

  def fromEitherException[A](ex: Throwable): IsEq[F[A]] =
    errors.fromEitherThrowable[A](Left(ex)) <-> errors.validationException(ex)

}

object ExtruderErrorsLaws {
  def apply[F[_]: Applicative: ExtruderErrors]: ExtruderErrorsLaws[F] = new ExtruderErrorsLaws[F] {
    override def F: Applicative[F] = Applicative[F]
    override def errors: ExtruderErrors[F] = ExtruderErrors[F]
  }
}
