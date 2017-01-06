package shapelessconfig.core

import cats.data.ValidatedNel

object validation {
  type ConfigValidation[T] = ValidatedNel[String, T]
}
