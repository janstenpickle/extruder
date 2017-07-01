package extruder.instances

import cats.FlatMap
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.syntax.validated._
import extruder.core.{ExtruderApplicativeError, Hints, IOF, IOFlatMap, Validation, ValidationErrors}

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

  implicit val extruderApplicativeErrorForValidation: ExtruderApplicativeError[Validation, ValidationErrors] =
    new AccumulatesErrors[Validation] {
      override def raiseError[A](e: ValidationErrors): Validation[A] = e.invalid
      override def handleErrorWith[A](fa: Validation[A])(f: (ValidationErrors) => Validation[A]): Validation[A] =
        fa.fold(f, _.validNel)
      override def pure[A](x: A): Validation[A] = x.validNel
      override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validation[B] =
        fa.ap(ff)
    }

  implicit val ioFlatMapForValidation: IOFlatMap[Validation] = new IOFlatMap[Validation] {
    override def flatMap[A, B](fa: IOF[Validation, A])(f: (A) => IOF[Validation, B]): IOF[Validation, B] =
      fa.flatMap {
        case Valid(v) => f(v)
        case x @ Invalid(_) => IO(x)
      }
  }
}
