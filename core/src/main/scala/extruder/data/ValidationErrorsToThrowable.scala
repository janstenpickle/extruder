package extruder.data
import extruder.data.ValidationError.ValidationException

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
