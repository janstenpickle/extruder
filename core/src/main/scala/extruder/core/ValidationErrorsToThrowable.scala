package extruder.core

import extruder.data.ValidationError.ValidationException
import extruder.data.{ValidationError, ValidationErrors}

/**
  * Converts a single or collection of validation errors to a throwable
  *
  * May be used with cats-effect or when performing an unsafe invocation
  */
trait ValidationErrorsToThrowable {
  def convertError(error: ValidationError): Throwable
  def convertErrors(errors: ValidationErrors): Throwable
}

object ValidationErrorsToThrowable {
  def apply(implicit validationErrorsThrowable: ValidationErrorsToThrowable): ValidationErrorsToThrowable =
    validationErrorsThrowable

  implicit val defaultValidationErrorsThrowable: ValidationErrorsToThrowable = new ValidationErrorsToThrowable {
    override def convertError(error: ValidationError): Throwable = error match {
      case e: ValidationException => e.exception
      case e: Any => e
    }

    override def convertErrors(errors: ValidationErrors): Throwable = errors.tail.foldLeft(convertError(errors.head)) {
      (acc, v) =>
        acc.addSuppressed(convertError(v))
        acc
    }
  }
}
