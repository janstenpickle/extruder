package extruder.laws

import cats.Eq
import extruder.core.{Parser, Show}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws
import cats.implicits._

trait ParserShowTests[A] extends Laws {
  def laws: ParserShowLaws[A]

  def parserShow(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet = new RuleSet {
    override def name: String = "parserShow"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Nil
    override def props: Seq[(String, Prop)] = Seq("parserShow showParse" -> forAll(laws.showParse _))
  }
}

object ParserShowTests {
  def apply[A: Parser: Show]: ParserShowTests[A] = new ParserShowTests[A] {
    override def laws: ParserShowLaws[A] = ParserShowLaws[A]
  }
}
