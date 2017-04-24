package extruder

import cats.data.ValidatedNel

package object core {
  type ConfigValidation[T] = ValidatedNel[ValidationError, T]
}
