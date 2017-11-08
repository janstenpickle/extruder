package extruder.core

import cats.{ApplicativeError, FlatMap}
import cats.effect.Effect
import scala.util.{Either, Failure, Success, Try}

abstract class ExtruderEffect[F[_], E](implicit E: Effect[F]) extends ApplicativeError[F, E] {
  def catchNonFatal[A](value: => A): F[A] = Try(value) match {
    case Failure(ex) => validationException(ex.getMessage, ex)
    case Success(v) => pure(v)
  }

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = E.flatMap(fa)(f)
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] = E.async(k)
  def suspend[A](thunk: => F[A]): F[A] = E.suspend(thunk)
}

object ExtruderEffect {
  class FromEffect[F[_]](implicit E: Effect[F]) extends ExtruderEffect[F, Throwable] {
    override def missing[A](message: String): F[A] = E.raiseError(new NoSuchElementException(message))

    override def validationFailure[A](message: String): F[A] = E.raiseError(new RuntimeException(message))

    override def validationException[A](message: String, ex: Throwable): F[A] = E.raiseError(ex)

    override def raiseError[A](e: Throwable): F[A] = E.raiseError(e)

    override def handleErrorWith[A](fa: F[A])(f: (Throwable) => F[A]): F[A] = E.handleErrorWith(fa)(f)

    override def pure[A](x: A): F[A] = E.pure(x)

    override def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] = E.ap(ff)(fa)
  }
}
