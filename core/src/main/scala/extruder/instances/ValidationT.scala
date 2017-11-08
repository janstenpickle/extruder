package extruder.instances

import extruder.core.Validation

case class ValidationT[F[_], A](value: F[Validation[A]]) {}
