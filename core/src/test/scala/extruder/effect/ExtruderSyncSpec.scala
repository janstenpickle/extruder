package extruder.effect

import cats.effect.laws.discipline.SyncTests
import cats.instances.all._
import org.specs2.specification.core.Fragments

abstract class ExtruderSyncSpec[F[_], E](implicit F: ExtruderSync[F]) extends EffectSpec[F, E] {
  override def ruleTest: Fragments = checkAll("ExtruderSync", SyncTests[F].sync[Int, Int, Int])
}
