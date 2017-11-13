package extruder.instances

import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.validated._
import extruder.core
import extruder.core.{ExtruderEffect, Missing, Validation, ValidationError, ValidationException, ValidationFailure}

trait ValidationInstances {
  implicit val validationEffect: ExtruderEffect[Validation] = new ExtruderEffect.FromApplicative[Validation]() {
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

    override def missing[A](message: String): ValidatedNel[Missing, A] = Missing(message).invalidNel

    override def validationFailure[A](message: String): ValidatedNel[ValidationFailure, A] =
      ValidationFailure(message).invalidNel

    override def validationException[A](message: String, ex: Throwable): ValidatedNel[ValidationException, A] =
      ValidationException(message, ex).invalidNel
  }
}
