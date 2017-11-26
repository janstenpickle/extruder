package extruder.data

import extruder.core.Validation

case class ValidationT[F[_], A](value: F[Validation[A]])
