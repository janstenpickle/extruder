package extruder.data

sealed trait ValidationError extends Exception {
  def message: String
  override def getMessage: String = message
}

case class ValidationException(message: String, exception: Throwable) extends ValidationError
object ValidationException {
  def apply(exception: Throwable): ValidationException = new ValidationException(exception.getMessage, exception)
}
case class ValidationFailure(message: String) extends ValidationError
case class Missing(message: String) extends ValidationError
