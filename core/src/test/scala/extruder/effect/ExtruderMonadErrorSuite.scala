package extruder.effect

import cats.instances.all._
import cats.laws.discipline.MonadErrorTests

abstract class ExtruderMonadErrorSuite[F[_], E](implicit F: ExtruderMonadError[F]) extends EffectSuite[F, E] {
  checkAll("ExtruderMonadError", MonadErrorTests[F, Throwable].monadError[Int, Int, Int])
}
