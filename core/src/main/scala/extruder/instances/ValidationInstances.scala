package extruder.instances

import cats.Applicative
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.{Effect, IO}
import cats.syntax.validated._
import extruder.core
import extruder.core.{ExtruderEffect, Missing, Validation, ValidationError, ValidationException, ValidationFailure}

import scala.util.{Failure, Success, Try}

trait ValidationInstances {
//  implicit val validationEffect: ExtruderEffect[Validation] = new ExtruderEffect.FromApplicative[Validation] {
//    override def missing[A](message: String): ValidatedNel[Missing, A] = Missing(message).invalidNel
//
//    override def validationFailure[A](message: String): ValidatedNel[ValidationFailure, A] =
//      ValidationFailure(message).invalidNel
//
//    override def validationException[A](message: String, ex: Throwable): ValidatedNel[ValidationException, A] =
//      ValidationException(message, ex).invalidNel
//
//    override def tailRecM[A, B](a: A)(f: (A) => Validation[Either[A, B]]): Validation[B] =
//      flatMap(f(a)) {
//        case Left(aa) => tailRecM(aa)(f)
//        case Right(b) => Valid(b)
//      }
//
//    override def flatMap[A, B](fa: Validation[A])(f: (A) => Validation[B]): Validation[B] = fa.fold(_.invalid, f)
//
//    override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validated[NonEmptyList[ValidationError], B] =
//      fa.ap(ff)
//
//    override def product[A, B](fa: Validation[A], fb: Validation[B]): Validation[(A, B)] = fa.product(fb)
//
//    override def raiseError[A](e: Throwable): Validation[A] = ValidationException(e.getMessage, e).invalidNel
//
//    override def handleErrorWith[A](fa: Validation[A])(f: (Throwable) => Validation[A]): Validation[A] =
//      fa // FIXME
//  }

  implicit val validationEffect: ExtruderEffect[Validation] = new ExtruderEffect[Validation] {
    protected def F: Applicative[Validation] = Applicative[Validation]

    override def runAsync[A](fa: Validation[A])(cb: (Either[Throwable, A]) => IO[Unit]): IO[Unit] =
      IO.pure(())

    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): Validation[A] =
      Try(IO.async(k).unsafeRunSync()) match {
        case Failure(ex) => raiseError(ex)
        case Success(a) => a.valid
      }

    override def suspend[A](thunk: => Validation[A]): Validation[A] = thunk

    override def tailRecM[A, B](a: A)(f: (A) => Validation[Either[A, B]]): Validation[B] =
      flatMap(f(a)) {
        case Left(aa) => tailRecM(aa)(f)
        case Right(b) => Valid(b)
      }

    override def flatMap[A, B](fa: Validation[A])(f: (A) => Validation[B]): Validation[B] = fa.fold(_.invalid, f)

    override def raiseError[A](e: Throwable): Validation[A] = ValidationException(e.getMessage, e).invalidNel

    override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validated[NonEmptyList[ValidationError], B] =
      fa.ap(ff)

    override def handleErrorWith[A](fa: Validation[A])(f: (Throwable) => Validation[A]): Validation[A] =
      fa.fold(e => f(core.errorsToThrowable(e)), Valid(_))

    override def product[A, B](fa: Validation[A], fb: Validation[B]): Validation[(A, B)] = fa.product(fb)

    override def pure[A](x: A): Validation[A] = Valid(x)

    override def missing[A](message: String) = Missing(message).invalidNel

    override def validationFailure[A](message: String) = ValidationFailure(message).invalidNel

    override def validationException[A](message: String, ex: Throwable) = ValidationException(message, ex).invalidNel
  }
}
