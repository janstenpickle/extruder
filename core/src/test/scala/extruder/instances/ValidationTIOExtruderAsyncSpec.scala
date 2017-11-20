package extruder.instances

import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.data.ValidationT
import extruder.effect.{ExtruderAsync, ExtruderAsyncSpec, ValidationTIOEffectSpec}
import extruder.instances.ValidationTIOExtruderAsyncSpec._

class ValidationTIOExtruderAsyncSpec extends ExtruderAsyncSpec[IOVal, ValidationErrors] with ValidationTIOEffectSpec

object ValidationTIOExtruderAsyncSpec {
  type IOVal[A] = ValidationT[IO, A]

  implicit val Instance: ExtruderAsync[IOVal] = ExtruderAsync.validationTAsync[IO]
}
