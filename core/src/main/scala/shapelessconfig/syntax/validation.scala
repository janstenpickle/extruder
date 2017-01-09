package shapelessconfig.syntax

import cats.data.ValidatedNel

object validation {
  type ConfigValidation[T] = ValidatedNel[String, T]
}
