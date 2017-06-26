package extruder.instances

import cats.FlatMap
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.syntax.validated._
import extruder.core.{ConfigValidation, ExtruderApplicativeError, IOF, IOFlatMap, Hints, ValidationErrors}

trait ConfigValidationInstances {
  import ExtruderApplicativeError._

  implicit protected val configValidationFlatMap: FlatMap[ConfigValidation] = new FlatMap[ConfigValidation] {
    override def tailRecM[A, B](a: A)(f: (A) => ConfigValidation[Either[A, B]]): ConfigValidation[B] =
      flatMap(f(a)) {
        case Left(aa) => tailRecM(aa)(f)
        case Right(b) => Valid(b)
      }

    override def flatMap[A, B](fa: ConfigValidation[A])(f: (A) => ConfigValidation[B]): ConfigValidation[B] =
      fa.fold(_.invalid, f)

    override def map[A, B](fa: ConfigValidation[A])(f: (A) => B): ConfigValidation[B] = fa.map(f)
  }

  implicit val extruderApplicativeErrorForConfigValidation: ExtruderApplicativeError[ConfigValidation, ValidationErrors] =
    new AccumulatesErrors[ConfigValidation] {
      override def raiseError[A](e: ValidationErrors): ConfigValidation[A] = e.invalid
      override def handleErrorWith[A](fa: ConfigValidation[A])(f: (ValidationErrors) => ConfigValidation[A]): ConfigValidation[A] = fa.fold(f, _.validNel)
      override def pure[A](x: A): ConfigValidation[A] = x.validNel
      override def ap[A, B](ff: ConfigValidation[(A) => B])(fa: ConfigValidation[A]): ConfigValidation[B] =
        fa.ap(ff)
    }

  implicit val ioFlatMapForConfigValidation: IOFlatMap[ConfigValidation] = new IOFlatMap[ConfigValidation] {
    override def flatMap[A, B](fa: IOF[ConfigValidation, A])(f: (A) => IOF[ConfigValidation, B]): IOF[ConfigValidation, B] =
      fa.flatMap {
        case Valid(v) => f(v)
        case x @ Invalid(_) => IO(x)
      }
  }
}
