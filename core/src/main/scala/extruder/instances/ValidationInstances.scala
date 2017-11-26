package extruder.instances

import cats.Eval
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.validated._
import extruder.core
import extruder.core.{Missing, Validation, ValidationError, ValidationException, ValidationFailure}
import extruder.data.ValidationT
import extruder.effect.{ExtruderMonadError, ExtruderSync}

import scala.util.control.NonFatal

trait ValidationInstances {
  implicit val validationMonadError: ExtruderMonadError[Validation] =
    new ExtruderMonadError[Validation] {
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

      override def pure[A](x: A) = x.validNel
    }

  implicit val validationTEvalSync: ExtruderSync[ValidationT[Eval, ?]] = new ExtruderSync[ValidationT[Eval, ?]] {
    override def suspend[A](thunk: => ValidationT[Eval, A]): ValidationT[Eval, A] =
      ValidationT[Eval, A](Eval.always(try {
        thunk.value.value
      } catch {
        case NonFatal(t) => ValidationException(t.getMessage, t).invalidNel
      }))

    override def validationException[A](message: String, ex: Throwable): ValidationT[Eval, A] = ???

    override def missing[A](message: String): ValidationT[Eval, A] = ???

    override def validationFailure[A](message: String): ValidationT[Eval, A] = ???

    override def raiseError[A](e: Throwable): ValidationT[Eval, A] = validationException(e.getMessage, e)

    override def handleErrorWith[A](
      fa: ValidationT[Eval, A]
    )(f: Throwable => ValidationT[Eval, A]): ValidationT[Eval, A] =
      ValidationT(
        fa.value.flatMap(_.fold(errs => f.andThen(_.value)(core.errorsToThrowable(errs)), a => Eval.now(Valid(a))))
      )

    override def pure[A](x: A): ValidationT[Eval, A] = ValidationT.liftF(Eval.now(x))

    override def tailRecM[A, B](a: A)(f: A => ValidationT[Eval, Either[A, B]]): ValidationT[Eval, B] = ???

    override def flatMap[A, B](fa: ValidationT[Eval, A])(f: A => ValidationT[Eval, B]): ValidationT[Eval, B] =
      ???
  }
}
