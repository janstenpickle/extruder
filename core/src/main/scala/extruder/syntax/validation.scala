package extruder.syntax

import cats.data.ValidatedNel
import extruder.core.ValidationFailure

object validation {
  type ConfigValidation[T] = ValidatedNel[ValidationFailure, T]
}
