package extruder.laws

import cats.data.{OptionT, ValidatedNel}
import cats.{Applicative, Eq}
import extruder.core.{MultiParser, MultiShow}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait MultiParserShowTests[F[_], A] extends Laws {
  def laws: MultiParserShowLaws[F, A]

  def parserShow(implicit arbA: Arbitrary[A], eqA: Eq[OptionT[F, ValidatedNel[String, A]]]): RuleSet = new RuleSet {
    override def name: String = "multiParserMultiShow"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Nil
    override def props: Seq[(String, Prop)] =
      Seq(
        "multiParserMultiShow showParse" -> forAll(laws.showParse _),
        "multiParserMultiShow parseEmpty" -> forAll(laws.parseEmpty _)
      )
  }
}

object MultiParserShowTests {
  def apply[F[_]: Applicative, A: MultiShow](implicit parser: MultiParser[F, A]): MultiParserShowTests[F, A] =
    new MultiParserShowTests[F, A] {
      override def laws: MultiParserShowLaws[F, A] = MultiParserShowLaws[F, A]
    }
}
