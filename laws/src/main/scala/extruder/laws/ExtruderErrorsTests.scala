package extruder.laws

import cats.{Applicative, Eq}
import extruder.core.ExtruderErrors
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait ExtruderErrorsTests[F[_]] extends Laws {
  def laws: ExtruderErrorsLaws[F]

  def extruderErrors[A: Arbitrary: Eq](
    implicit arbFA: Arbitrary[F[A]],
    arbTh: Arbitrary[Throwable],
    eqFa: Eq[F[A]]
  ): RuleSet = new RuleSet {
    override def name: String = "extruderErrors"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Nil
    override def props: Seq[(String, Prop)] = Seq(
      "extruderErrors fallbackWithPure" -> forAll(laws.fallbackWithPure[A] _),
      "extruderErrors validationException" -> forAll(laws.validationException[A] _),
      "extruderErrors fromTry" -> forAll(laws.fromTry[A] _),
      "extruderErrors fromEitherException" -> forAll(laws.fromEitherException[A] _)
    )
  }
}

object ExtruderErrorsTests {
  def apply[F[_]: Applicative: ExtruderErrors]: ExtruderErrorsTests[F] = new ExtruderErrorsTests[F] {
    override def laws: ExtruderErrorsLaws[F] = ExtruderErrorsLaws[F]
  }
}
