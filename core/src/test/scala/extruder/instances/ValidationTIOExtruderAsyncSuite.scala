package extruder.instances

import cats.data.EitherT
import cats.effect.IO
import extruder.core.ValidationErrors
import extruder.effect.{ExtruderAsync, ExtruderAsyncSuite, ValidationTIOEffectSuite}
import extruder.instances.ValidationTIOExtruderAsyncSuite._

class ValidationTIOExtruderAsyncSuite extends ExtruderAsyncSuite[IOVal, ValidationErrors] with ValidationTIOEffectSuite

object ValidationTIOExtruderAsyncSuite {
  type IOVal[A] = EitherT[IO, ValidationErrors, A]

  implicit val Instance: ExtruderAsync[IOVal] = ExtruderAsync.eitherTAsync[IO]
}
