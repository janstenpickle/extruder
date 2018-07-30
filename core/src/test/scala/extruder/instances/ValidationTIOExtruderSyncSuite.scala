package extruder.instances

import cats.data.EitherT
import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.effect.{ExtruderSync, ExtruderSyncSuite, ValidationTIOEffectSuite}
import extruder.instances.ValidationTIOExtruderSyncSuite._

class ValidationTIOExtruderSyncSuite extends ExtruderSyncSuite[IOVal, ValidationErrors] with ValidationTIOEffectSuite

object ValidationTIOExtruderSyncSuite {
  type IOVal[A] = EitherT[IO, ValidationErrors, A]

  implicit val Instance: ExtruderSync[IOVal] = ExtruderSync.eitherTSync[IO]
}
