package extruder.instances

import cats.data.EitherT
import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.effect.{ExtruderMonadError, ExtruderMonadErrorSuite, ValidationTIOEffectSuite}
import extruder.instances.ValidationTIOExtruderMonadErrorSuite._

class ValidationTIOExtruderMonadErrorSuite
    extends ExtruderMonadErrorSuite[IOVal, ValidationErrors]
    with ValidationTIOEffectSuite

object ValidationTIOExtruderMonadErrorSuite {
  type IOVal[A] = EitherT[IO, ValidationErrors, A]

  implicit val Instance: ExtruderMonadError[IOVal] = ExtruderMonadError.eitherTMonadError[IO]
}
