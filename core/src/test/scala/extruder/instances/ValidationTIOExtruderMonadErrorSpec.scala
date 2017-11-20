package extruder.instances

import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.data.ValidationT
import extruder.effect.{ExtruderMonadError, ExtruderMonadErrorSpec, ValidationTIOEffectSpec}
import extruder.instances.ValidationTIOExtruderMonadErrorSpec._

class ValidationTIOExtruderMonadErrorSpec
    extends ExtruderMonadErrorSpec[IOVal, ValidationErrors]
    with ValidationTIOEffectSpec

object ValidationTIOExtruderMonadErrorSpec {
  type IOVal[A] = ValidationT[IO, A]

  implicit val Instance: ExtruderMonadError[IOVal] = ExtruderMonadError.validationTMonadError[IO]
}
