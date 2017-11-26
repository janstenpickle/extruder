package extruder

import cats.data.{EitherT, NonEmptyList, ValidatedNel}

package object core {
  type EitherThrowable[T] = Either[Throwable, T]
  type EitherErrors[T] = Either[ValidationErrors, T]
  type ValidationErrors = NonEmptyList[ValidationError]
  type Validation[T] = Either[ValidationErrors, T]
  type ValidationT[F[_], T] = EitherT[F, ValidationErrors, T]

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
