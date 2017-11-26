package extruder.effect

import cats.instances.all._
import cats.laws.discipline.MonadErrorTests
import org.specs2.specification.core.SpecStructure

abstract class ExtruderMonadErrorSpec[F[_]](implicit F: ExtruderMonadError[F]) extends EffectSpec[F] {
  lazy val tests: MonadErrorTests[F, Throwable] = MonadErrorTests[F, Throwable]

  override def is: SpecStructure = checkAll("ExtruderMonadError", tests.monadError[Int, Int, Int])
}
