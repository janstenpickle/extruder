package extruder.laws

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyStream, NonEmptyVector}
import cats.{Eq, Monad, Monoid, Order}
import extruder.core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderMapTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderMultiTests[F, S, E, D, O] {
  implicit def mapArb[A, B](implicit arb: Arbitrary[List[(A, B)]]): Arbitrary[Map[A, B]] =
    Arbitrary(arb.arbitrary.suchThat(_.nonEmpty).map(_.toMap))

  def mapEncodeDecode[A: Arbitrary: Parser: Show: Order, B: Arbitrary: Parser: Show, C: Arbitrary](
    implicit eqFA: Eq[F[A]],
    eqFEitherAC: Eq[F[Either[C, A]]],
    eqFListA: Eq[F[List[A]]],
    encoder: Encoder[F, S, A, E],
    decoder: Decoder[F, S, A, O],
    cEncoder: Encoder[F, S, C, E],
    cDecoder: Decoder[F, S, C, O],
    eqFOptA: Eq[F[Option[A]]],
    optEncoder: Encoder[F, S, Option[A], E],
    optDecoder: Decoder[F, S, Option[A], O],
    eqFChainA: Eq[F[Chain[A]]],
    eqFNelA: Eq[F[NonEmptyList[A]]],
    eqFNevA: Eq[F[NonEmptyVector[A]]],
    eqFNecA: Eq[F[NonEmptyChain[A]]],
    eqFNeSetA: Eq[F[NonEmptySet[A]]],
    eqFNeStreamA: Eq[F[NonEmptyStream[A]]],
    chainEncoder: Encoder[F, S, Chain[A], E],
    chainDecoder: Decoder[F, S, Chain[A], O],
    nelEncoder: Encoder[F, S, NonEmptyList[A], E],
    nelDecoder: Decoder[F, S, NonEmptyList[A], O],
    nevEncoder: Encoder[F, S, NonEmptyVector[A], E],
    nevDecoder: Decoder[F, S, NonEmptyVector[A], O],
    necEncoder: Encoder[F, S, NonEmptyChain[A], E],
    necDecoder: Decoder[F, S, NonEmptyChain[A], O],
    neSetEncoder: Encoder[F, S, NonEmptySet[A], E],
    neSetDecoder: Decoder[F, S, NonEmptySet[A], O],
    neStreamEncoder: Encoder[F, S, NonEmptyStream[A], E],
    neStreamDecoder: Decoder[F, S, NonEmptyStream[A], O],
    listDecoder: Decoder[F, S, List[A], O],
    optStringDecoder: Decoder[F, S, Option[String], O],
    stringEncoder: Encoder[F, S, String, E],
    eqFMultiClass: Eq[F[MultiClass[A, B]]],
    optEqFMultiClass: Eq[F[Option[MultiClass[A, B]]]],
    mapEncoder: Encoder[F, S, Map[B, A], E],
    mapDecoder: Decoder[F, S, Map[B, A], O],
    eqFMapBA: Eq[F[Map[B, A]]]
  ): RuleSet = new RuleSet {
    override def name: String = "mapEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(multiEncodeDecode[A, B, C])
    override def props: Seq[(String, Prop)] =
      Seq(
        "map encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Map[B, A]] _),
        "map encodeDecodeWithPartiallyApplied" -> forAll(laws.encodeDecodeWithPartiallyApplied[Map[B, A]] _)
      )
  }
}

object EncoderDecoderMapTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](settings: S)(
    implicit fin: Transform[F, S, E, D],
    prep: Transform[F, S, D, O],
    hv: HasValue[F, S, O]
  ): EncoderDecoderMapTests[F, S, E, D, O] =
    new EncoderDecoderMapTests[F, S, E, D, O] {
      override def F: Monad[F] = Monad[F]
      override def monoid: Monoid[E] = Monoid[E]
      override def errors: ExtruderErrors[F] = ExtruderErrors[F]
      override def laws: EncoderDecoderDerivedLaws[F, S, E, D, O] = EncoderDecoderDerivedLaws[F, S, E, D, O](settings)
    }
}
