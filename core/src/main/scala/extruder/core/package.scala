package extruder

import cats.data.ValidatedNel

package object core {
  type ParametersMap = Map[Seq[String], (Option[String], String, Boolean)]
  type ConfigValidation[T] = ValidatedNel[ValidationError, T]
}
