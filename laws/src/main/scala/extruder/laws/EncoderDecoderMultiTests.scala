package extruder.laws

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.{Eq, Monad, Monoid}
import extruder.core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderMultiTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderDerivedTests[F, S, E, D, O] {
  def multiEncodeDecode[A: Arbitrary, B: Arbitrary](
    implicit eqFA: Eq[F[A]],
    eqFListA: Eq[F[List[A]]],
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
    necDecoder: DecoderT[F, S, NonEmptyChain[A], O],
    listDecoder: DecoderT[F, S, List[A], O],
    eqFMapBA: Eq[F[Map[B, A]]],
    multiShowEncoder: EncoderT[F, S, (A, B), E],
    multiShowDecoder: DecoderT[F, S, (A, B), O],
    eqFTuple: Eq[F[(A, B)]],
    optMultiShowEncoder: EncoderT[F, S, Option[(A, B)], E],
    optMultiShowDecoder: DecoderT[F, S, Option[(A, B)], O],
    optEqFTuple: Eq[F[Option[(A, B)]]]
  ): RuleSet = new RuleSet {
    override def name: String = "mutliEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(derivedEncodeDecode[A])
    override def props: Seq[(String, Prop)] =
      Seq(
        "tuple encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[(A, B)] _),
        "tuple encodeDecodeWithPartiallyApplied" -> forAll(laws.encodeDecodeWithPartiallyApplied[(A, B)] _),
        "optional tuple encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Option[(A, B)]] _),
        "optional tuple encodeDecodeWithPartiallyApplied" -> forAll(
          laws.encodeDecodeWithPartiallyApplied[Option[(A, B)]] _
        )
      )
  }
}

object EncoderDecoderMultiTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](
    settings: S
  )(implicit fin: Transform[F, S, E, D], prep: Transform[F, S, D, O]): EncoderDecoderMultiTests[F, S, E, D, O] =
    new EncoderDecoderMultiTests[F, S, E, D, O] {
      override def laws: EncoderDecoderLaws[F, S, E, D, O] = EncoderDecoderLaws[F, S, E, D, O](settings)
    }
}
