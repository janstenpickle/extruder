package extruder.syntax

import cats.syntax.either._
import extruder.core.{ConfigValidation, ValidationException}

package object validation {
  implicit class Handle[T](value: => T) {
    def handle: ConfigValidation[T] = Either.catchNonFatal(value)
      .leftMap(ex => new ValidationException(ex.getMessage, ex))
      .toValidatedNel
  }
}
