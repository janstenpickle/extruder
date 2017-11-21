package extruder.instances

import cats.data.EitherT
import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.effect.{ExtruderMonadError, ExtruderMonadErrorSpec, ValidationTIOEffectSpec}
import extruder.instances.ValidationTIOExtruderMonadErrorSpec._

class ValidationTIOExtruderMonadErrorSpec
    extends ExtruderMonadErrorSpec[IOVal, ValidationErrors]
    with ValidationTIOEffectSpec

object ValidationTIOExtruderMonadErrorSpec {
  type IOVal[A] = EitherT[IO, ValidationErrors, A]

  implicit val Instance: ExtruderMonadError[IOVal] = ExtruderMonadError.eitherTMonadError[IO]
}
