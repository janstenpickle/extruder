package shapelessconfig.syntax

import cats.data.ValidatedNel
import shapelessconfig.core.ValidationFailure

object validation {
  type ConfigValidation[T] = ValidatedNel[ValidationFailure, T]
}
