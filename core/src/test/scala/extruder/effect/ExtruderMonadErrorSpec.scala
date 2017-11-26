package extruder.effect

import cats.instances.all._
import cats.laws.discipline.MonadErrorTests
import org.specs2.specification.core.Fragments

abstract class ExtruderMonadErrorSpec[F[_], E](implicit F: ExtruderMonadError[F]) extends EffectSpec[F, E] {
  override def ruleTest: Fragments =
    checkAll("ExtruderMonadError", MonadErrorTests[F, Throwable].monadError[Int, Int, Int])
}
