package extruder.instances

import cats.Eq
import extruder.core.Validation
import extruder.effect.ExtruderMonadErrorSpec

class ValidationExtruderMonadErrorSpec extends ExtruderMonadErrorSpec[Validation] {
  override implicit def feq[A](implicit eq: Eq[A]): Eq[Validation[A]] = Eq.fromUniversalEquals
}
