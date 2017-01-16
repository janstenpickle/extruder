package extruder.core

import cats.syntax.validated._
import extruder.syntax.validation.ConfigValidation

case class ValidationFailure(message: String, exception: Option[Throwable])

object ValidationFailure {
  def apply[T](message: String, exception: Option[Throwable] = None): ConfigValidation[T] =
    new ValidationFailure(message, exception).invalidNel[T]
}
