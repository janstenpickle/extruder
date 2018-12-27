package extruder.laws

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.{Eq, Monad, Monoid}
import extruder.core.{DecoderT, EncoderT, ExtruderErrors, Settings}
import extruder.data.Transform
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderDerivedTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderTests[F, S, E, D, O] {
  implicit def chainArb[A](implicit listArb: Arbitrary[List[A]]): Arbitrary[Chain[A]] =
    Arbitrary(listArb.arbitrary.map(Chain.fromSeq))
  implicit def nonEmptyChainArb[A](implicit nelArb: Arbitrary[NonEmptyList[A]]): Arbitrary[NonEmptyChain[A]] =
    Arbitrary(nelArb.arbitrary.map(NonEmptyChain.fromNonEmptyList))
  implicit def nonEmptyVectorArb[A](implicit nelArb: Arbitrary[NonEmptyChain[A]]): Arbitrary[NonEmptyVector[A]] =
    Arbitrary(nelArb.arbitrary.map(_.toNonEmptyVector))

  def derivedEncodeDecode[A: Arbitrary](
    implicit eqFA: Eq[F[A]],
    encoder: EncoderT[F, S, A, E],
    decoder: DecoderT[F, S, A, O],
    eqFOptA: Eq[F[Option[A]]],
    optEncoder: EncoderT[F, S, Option[A], E],
    optDecoder: DecoderT[F, S, Option[A], O],
    eqFChainA: Eq[F[Chain[A]]],
    eqFNelA: Eq[F[NonEmptyList[A]]],
    eqFNevA: Eq[F[NonEmptyVector[A]]],
    eqFNecA: Eq[F[NonEmptyChain[A]]],
    chainEncoder: EncoderT[F, S, Chain[A], E],
    chainDecoder: DecoderT[F, S, Chain[A], O],
    nelEncoder: EncoderT[F, S, NonEmptyList[A], E],
    nelDecoder: DecoderT[F, S, NonEmptyList[A], O],
    nevEncoder: EncoderT[F, S, NonEmptyVector[A], E],
    nevDecoder: DecoderT[F, S, NonEmptyVector[A], O],
    necEncoder: EncoderT[F, S, NonEmptyChain[A], E],
    necDecoder: DecoderT[F, S, NonEmptyChain[A], O]
  ): RuleSet = new RuleSet {
    override def name: String = "derivedEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(encodeDecode[A])
    override def props: Seq[(String, Prop)] = Seq(
      "option encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Option[A]] _),
      "chain encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Chain[A]] _),
      "non-empty list encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyList[A]] _),
      "non-empty vector encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyVector[A]] _),
      "non-empty chain encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyChain[A]] _)
    )
  }
}

object EncoderDecoderDerivedTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](
    settings: S
  )(implicit fin: Transform[F, S, E, D], prep: Transform[F, S, D, O]): EncoderDecoderDerivedTests[F, S, E, D, O] =
    new EncoderDecoderDerivedTests[F, S, E, D, O] {
      override def laws: EncoderDecoderLaws[F, S, E, D, O] = EncoderDecoderLaws[F, S, E, D, O](settings)
    }
}
