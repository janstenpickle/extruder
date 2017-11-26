package extruder

import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.IO

package object core {
  implicit object `decode disambiguator`
  implicit object `decode without input disambiguator`

  type EitherThrowable[T] = Either[Throwable, T]
  type EitherErrors[T] = Either[ValidationErrors, T]
  type ValidationErrors = NonEmptyList[ValidationError]
  type Validation[T] = ValidatedNel[ValidationError, T]
  type IOF[F[_], T] = IO[F[T]]

  val TypeKey: String = "type"

  def errorsToThrowable(errs: ValidationErrors): Throwable = {
    val errorToThrowable: ValidationError => Throwable = {
      case e: ValidationException => e.exception
      case e: Any => new RuntimeException(e.message)
    }

    errs.tail.foldLeft(errorToThrowable(errs.head)) { (acc, v) =>
      acc.addSuppressed(errorToThrowable(v))
      acc
    }
  }
}
