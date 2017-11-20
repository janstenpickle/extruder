package extruder.instances

import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.data.ValidationT
import extruder.effect.{ExtruderSync, ExtruderSyncSpec, ValidationTIOEffectSpec}
import extruder.instances.ValidationTIOExtruderSyncSpec._

class ValidationTIOExtruderSyncSpec extends ExtruderSyncSpec[IOVal, ValidationErrors] with ValidationTIOEffectSpec

object ValidationTIOExtruderSyncSpec {
  type IOVal[A] = ValidationT[IO, A]

  implicit val Instance: ExtruderSync[IOVal] = ExtruderSync.validationTSync[IO]
}
