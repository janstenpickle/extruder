package extruder.laws

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.{Eq, Monad, Monoid}
import extruder.core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderMapTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderMultiTests[F, S, E, D, O] {
  implicit def mapArb[A, B](implicit arb: Arbitrary[List[(A, B)]]): Arbitrary[Map[A, B]] =
    Arbitrary(arb.arbitrary.suchThat(_.nonEmpty).map(_.toMap))

  def mapEncodeDecode[A: Arbitrary, B: Arbitrary](
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
    necDecoder: DecoderT[F, S, NonEmptyChain[A], O],
    multiShowEncoder: EncoderT[F, S, (A, B), E],
    multiShowDecoder: DecoderT[F, S, (A, B), O],
    eqFTuple: Eq[F[(A, B)]],
    optMultiShowEncoder: EncoderT[F, S, Option[(A, B)], E],
    optMultiShowDecoder: DecoderT[F, S, Option[(A, B)], O],
    optEqFTuple: Eq[F[Option[(A, B)]]],
    mapEncoder: EncoderT[F, S, Map[B, A], E],
    mapDecoder: DecoderT[F, S, Map[B, A], O],
    eqFMapBA: Eq[F[Map[B, A]]]
  ): RuleSet = new RuleSet {
    override def name: String = "mapEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(multiEncodeDecode[A, B])
    override def props: Seq[(String, Prop)] =
      Seq(
        "map encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Map[B, A]] _),
        "map encodeDecodeWithPartiallyApplied" -> forAll(laws.encodeDecodeWithPartiallyApplied[Map[B, A]] _)
      )
  }
}

object EncoderDecoderMapTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](
    settings: S
  )(implicit fin: Transform[F, S, E, D], prep: Transform[F, S, D, O]): EncoderDecoderMapTests[F, S, E, D, O] =
    new EncoderDecoderMapTests[F, S, E, D, O] {
      override def laws: EncoderDecoderLaws[F, S, E, D, O] = EncoderDecoderLaws[F, S, E, D, O](settings)
    }
}
