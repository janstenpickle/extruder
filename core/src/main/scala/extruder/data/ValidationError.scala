package extruder.data

import cats.Eq
import cats.instances.string._

import scala.util.control.NoStackTrace

sealed trait ValidationError extends Exception with NoStackTrace {
  def message: String
  override def getMessage: String = message
}

object ValidationError {
  case class ValidationException(message: String, exception: Throwable) extends ValidationError
  object ValidationException {
    def apply(exception: Throwable): ValidationException = new ValidationException(exception.getMessage, exception)
  }
  case class ValidationFailure(message: String) extends ValidationError
  case class Missing(message: String) extends ValidationError

  def exception(message: String, ex: Throwable): ValidationException = ValidationException(message, ex)
  def exception(ex: Throwable): ValidationException = ValidationException(ex)
  def failure(message: String): ValidationFailure = ValidationFailure(message)
  def missing(message: String): Missing = Missing(message)

  implicit val eq: Eq[ValidationError] = Eq.by(_.message)
}
