package extruder.core

sealed trait ValidationError {
  def message: String
}

case class ValidationException(message: String, exception: Throwable) extends ValidationError
case class ValidationFailure(message: String) extends ValidationError
case class Missing(message: String) extends ValidationError
