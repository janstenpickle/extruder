package extruder

import cats.data.ValidatedNel

package object core {
  type ConfigValidation[T] = ValidatedNel[ValidationFailure, T]
  type Parser[T] = String => Either[Throwable, T]
}
