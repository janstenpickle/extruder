package extruder.effect

import cats.effect.laws.discipline.SyncTests
import cats.instances.all._
import org.specs2.specification.core.SpecStructure

abstract class ExtruderSyncSpec[F[_]](implicit F: ExtruderSync[F]) extends EffectSpec[F] {
  lazy val tests: SyncTests[F] = SyncTests[F]

  override def is: SpecStructure = checkAll("ExtruderSync", tests.sync[Int, Int, Int])
}
