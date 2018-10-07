package extruder

import cats.data.{EitherT, NonEmptyList}

package object core {
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

  implicit object Dis
  implicit object Dis2
}
