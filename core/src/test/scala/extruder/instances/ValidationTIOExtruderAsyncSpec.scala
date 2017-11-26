package extruder.instances

import cats.data.EitherT
import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.effect.{ExtruderAsync, ExtruderAsyncSpec, ValidationTIOEffectSpec}
import extruder.instances.ValidationTIOExtruderAsyncSpec._

class ValidationTIOExtruderAsyncSpec extends ExtruderAsyncSpec[IOVal, ValidationErrors] with ValidationTIOEffectSpec

object ValidationTIOExtruderAsyncSpec {
  type IOVal[A] = EitherT[IO, ValidationErrors, A]

  implicit val Instance: ExtruderAsync[IOVal] = ExtruderAsync.eitherTAsync[IO]
}
