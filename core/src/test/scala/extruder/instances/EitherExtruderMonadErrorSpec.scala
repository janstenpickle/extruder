package extruder.instances

import cats.Eq
import extruder.core.ValidationError
import extruder.effect.ExtruderMonadErrorSpec

class EitherExtruderMonadErrorSpec extends ExtruderMonadErrorSpec[Either[ValidationError, ?]] {
  override implicit def feq[A](implicit eq: Eq[A]): Eq[Either[ValidationError, A]] = Eq.fromUniversalEquals
}
