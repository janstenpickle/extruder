package extruder.laws

import cats.{Applicative, Eq, Monoid}
import extruder.core.{DecoderT, EncoderT, ExtruderErrors, Settings}
import extruder.data.PathElement
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.typelevel.discipline.Laws

trait DecoderTests[F[_], S <: Settings, E, D, O] extends Laws {
  def laws: DecoderLaws[F, S, E, D, O]

  implicit val nonEmptyStringArb: Arbitrary[String] = Arbitrary(Gen.alphaNumStr.suchThat(_.nonEmpty))
  implicit val namespaceArb: Arbitrary[List[String]] = Arbitrary(Gen.nonEmptyListOf(nonEmptyStringArb.arbitrary))
  implicit val pathElemsArb: Arbitrary[List[PathElement]] = Arbitrary(
    Gen.nonEmptyListOf(nonEmptyStringArb.arbitrary.map(PathElement.Standard))
  )

  def decode[A: Arbitrary](
    implicit eqFa: Eq[F[A]],
    eqFListA: Eq[F[List[A]]],
    encoder: EncoderT[F, S, A, E],
    decoder: DecoderT[F, S, A, O],
    listDecoder: DecoderT[F, S, List[A], O]
  ): RuleSet = new RuleSet {
    override def name: String = "decoder"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Nil
    override def props: Seq[(String, Prop)] = Seq(
      "decoder decodeEmpty" -> forAll(laws.decodeEmpty[A] _),
      "decoder decodeDefault" -> forAll(laws.decodeDefault[A] _),
      "decoder decodeDefaultList" -> forAll(laws.decodeDefault[List[A]] _)
    )
  }
}

object DecoderTests {
  def apply[F[_]: Applicative: ExtruderErrors, S <: Settings, E, D, O: Monoid](
    settings: S
  ): DecoderTests[F, S, E, D, O] =
    new DecoderTests[F, S, E, D, O] {
      override def laws: DecoderLaws[F, S, E, D, O] = DecoderLaws[F, S, E, D, O](settings)
    }
}
