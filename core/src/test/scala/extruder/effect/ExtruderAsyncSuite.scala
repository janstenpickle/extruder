package extruder.effect

import cats.effect.laws.discipline.AsyncTests
import cats.instances.all._

abstract class ExtruderAsyncSuite[F[_], E](implicit F: ExtruderAsync[F]) extends EffectSuite[F, E] {
  checkAllAsync("ExtruderAsync", implicit ec => AsyncTests[F].async[Int, Int, Int])
}
