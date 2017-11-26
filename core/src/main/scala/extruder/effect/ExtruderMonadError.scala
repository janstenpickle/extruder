package extruder.effect

import cats.{Applicative, Monad, MonadError}
import extruder.instances.ValidationInstances

import scala.util.Either

trait ExtruderMonadError[F[_]] extends MonadError[F, Throwable] {
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]
}

//trait ExtruderMonadErrorInstances {
//  abstract class FromApplicative[F[_]](implicit F: Applicative[F]) extends ExtruderMonadError[F] {
//    override def pure[A](x: A): F[A] = F.pure(x)
//  }
//
//  abstract class FromMonad[F[_]](implicit F: Monad[F]) extends FromApplicative[F] {
//    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
//    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
//  }
//
//  implicit def fromMonadError[F[_]](implicit F: MonadError[F, Throwable]): ExtruderMonadError[F] =
//    new ExtruderMonadError[F] {
//      override def missing[A](message: String): F[A] = raiseError(new NoSuchElementException(message))
//      override def validationFailure[A](message: String): F[A] = raiseError(new RuntimeException(message))
//      override def validationException[A](message: String, ex: Throwable): F[A] = raiseError(ex)
//      override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
//      override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)
//
//      override def pure[A](x: A) = F.pure(x)
//
//      override def flatMap[A, B](fa: F[A])(f: A => F[B]) = F.flatMap(fa)(f)
//
//      override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]) = F.tailRecM(a)(f)
//    }
//}

object ExtruderMonadError extends ValidationInstances {
  implicit def fromMonadError[F[_]](implicit F: MonadError[F, Throwable]): ExtruderMonadError[F] =
    new ExtruderMonadError[F] {
      override def missing[A](message: String): F[A] = raiseError(new NoSuchElementException(message))
      override def validationFailure[A](message: String): F[A] = raiseError(new RuntimeException(message))
      override def validationException[A](message: String, ex: Throwable): F[A] = raiseError(ex)
      override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
      override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)

      override def pure[A](x: A) = F.pure(x)

      override def flatMap[A, B](fa: F[A])(f: A => F[B]) = F.flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]) = F.tailRecM(a)(f)
    }

  def apply[F[_]](implicit monadError: ExtruderMonadError[F]): ExtruderMonadError[F] = monadError
}
