package extruder.instances

import cats.data.EitherT
import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.effect.{ExtruderSync, ExtruderSyncSpec, ValidationTIOEffectSpec}
import extruder.instances.ValidationTIOExtruderSyncSpec._

class ValidationTIOExtruderSyncSpec extends ExtruderSyncSpec[IOVal, ValidationErrors] with ValidationTIOEffectSpec

object ValidationTIOExtruderSyncSpec {
  type IOVal[A] = EitherT[IO, ValidationErrors, A]

  implicit val Instance: ExtruderSync[IOVal] = ExtruderSync.eitherTSync[IO]
}
