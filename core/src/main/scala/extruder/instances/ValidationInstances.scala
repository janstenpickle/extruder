package extruder.instances

import cats.{FlatMap, Traverse}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.syntax.validated._
import extruder.core.{
  ExtruderApplicativeError,
  ExtruderEffect,
  Hints,
  IOF,
  IOFlatMap,
  Missing,
  Validation,
  ValidationError,
  ValidationErrors,
  ValidationException,
  ValidationFailure
}
import cats.instances._

trait ValidationInstances {
  import ExtruderApplicativeError._

  implicit protected val validationFlatMap: FlatMap[Validation] = new FlatMap[Validation] {
    override def tailRecM[A, B](a: A)(f: (A) => Validation[Either[A, B]]): Validation[B] =
      flatMap(f(a)) {
        case Left(aa) => tailRecM(aa)(f)
        case Right(b) => Valid(b)
      }

    override def flatMap[A, B](fa: Validation[A])(f: (A) => Validation[B]): Validation[B] =
      fa.fold(_.invalid, f)

    override def map[A, B](fa: Validation[A])(f: (A) => B): Validation[B] = fa.map(f)
  }

//  implicit val extruderApplicativeErrorForValidation: ExtruderApplicativeError[Validation, ValidationErrors] =
//    new AccumulatesErrors[Validation] {
//      override def raiseError[A](e: ValidationErrors): Validation[A] = e.invalid
//      override def handleErrorWith[A](fa: Validation[A])(f: (ValidationErrors) => Validation[A]): Validation[A] =
//        fa.fold(f, _.validNel)
//      override def pure[A](x: A): Validation[A] = x.validNel
////      override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validation[B] =
////        fa.ap(ff)
//    }

  implicit val extruderApplicativeErrorForValidation: ExtruderApplicativeError[Validation, ValidationErrors] =
    new ExtruderApplicativeError[Validation, ValidationErrors] {
      override def missing[A](message: String) = raiseError[A](NonEmptyList.of(Missing(message)))

      override def validationFailure[A](message: String) = raiseError[A](NonEmptyList.of(ValidationFailure(message)))

      override def validationException[A](message: String, ex: Throwable) =
        raiseError[A](NonEmptyList.of(ValidationException(message, ex)))

      override def raiseError[A](e: ValidationErrors) = e.invalid

      override def handleErrorWith[A](fa: Validation[A])(f: (ValidationErrors) => Validation[A]) =
        fa.fold(f, _.validNel)

      override def pure[A](x: A) = x.validNel

      override def product[A, B](fa: Validation[A], fb: Validation[B]): Validation[(A, B)] = fa.product(fb)

      override def flatMap[A, B](fa: Validation[A])(f: (A) => Validation[B]): Validation[B] =
        fa.fold(_.invalid, f)

      override def map[A, B](fa: Validation[A])(f: (A) => B) = fa.map(f)

      override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]) = fa.ap(ff)

      override def sequence[G[_], A](as: G[Validation[A]])(implicit G: Traverse[G]) = G.sequence(as)
    }

  implicit val ioFlatMapForValidation: IOFlatMap[Validation] = new IOFlatMap[Validation] {
    override def flatMap[A, B](fa: IOF[Validation, A])(f: (A) => IOF[Validation, B]): IOF[Validation, B] =
      fa.flatMap {
        case Valid(v) => f(v)
        case x @ Invalid(_) => IO(x)
      }
  }

//  implicit val extruderApplicativeErrorForValidation2: ExtruderEffect[Validation, ValidationErrors] =
//    new ExtruderEffect[Validation, ValidationErrors] {
//      override def missing[A](message: String) = raiseError[A](NonEmptyList.of(Missing(message)))
//
//      override def validationFailure[A](message: String) = raiseError[A](NonEmptyList.of(ValidationFailure(message)))
//
//      override def validationException[A](message: String, ex: Throwable) =
//        raiseError[A](NonEmptyList.of(ValidationException(message, ex)))
//
//      override def raiseError[A](e: ValidationErrors): Validated[ValidationErrors, Nothing] = e.invalid
//
//      override def handleErrorWith[A](fa: Validation[A])(f: (ValidationErrors) => Validation[A]): Validation[A] =
//        fa.fold(f, _.validNel)
//
//      override def pure[A](x: A): ValidatedNel[Nothing, A] = x.validNel
//
//      override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validated[NonEmptyList[ValidationError], B] =
//        fa.ap(ff)
//    }

}
