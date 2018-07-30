package extruder.effect

import cats.effect.laws.discipline.SyncTests
import cats.instances.all._

abstract class ExtruderSyncSuite[F[_], E](implicit F: ExtruderSync[F]) extends EffectSuite[F, E] {
  checkAllAsync("ExtruderSync", implicit ec => SyncTests[F].sync[Int, Int, Int])
}
