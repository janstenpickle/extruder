package extruder.laws

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyStream, NonEmptyVector}
import cats.{Eq, Monad, Monoid, Order}
import extruder.core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderDerivedTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderTests[F, S, E, D, O] {
  override def laws: EncoderDecoderDerivedLaws[F, S, E, D, O]

  implicit def chainArb[A](implicit listArb: Arbitrary[List[A]]): Arbitrary[Chain[A]] =
    Arbitrary(listArb.arbitrary.map(Chain.fromSeq))
  implicit def nonEmptyChainArb[A](implicit nelArb: Arbitrary[NonEmptyList[A]]): Arbitrary[NonEmptyChain[A]] =
    Arbitrary(nelArb.arbitrary.map(NonEmptyChain.fromNonEmptyList))
  implicit def nonEmptyVectorArb[A](implicit nelArb: Arbitrary[NonEmptyChain[A]]): Arbitrary[NonEmptyVector[A]] =
    Arbitrary(nelArb.arbitrary.map(_.toNonEmptyVector))
  implicit def nonEmptySetArb[A: Order](implicit nelArb: Arbitrary[NonEmptyChain[A]]): Arbitrary[NonEmptySet[A]] =
    Arbitrary(nelArb.arbitrary.map(c => NonEmptySet.of(c.head, c.tail.toVector: _*)))
  implicit def nonEmptyStreamArb[A](implicit nelArb: Arbitrary[NonEmptyChain[A]]): Arbitrary[NonEmptyStream[A]] =
    Arbitrary(nelArb.arbitrary.map(c => NonEmptyStream(c.head, c.tail.toVector: _*)))

  def derivedEncodeDecode[A: Arbitrary: Order, B: Arbitrary](
    implicit eqFA: Eq[F[A]],
    eqFEitherAB: Eq[F[Either[B, A]]],
    aEncoder: Encoder[F, S, A, E],
    aDecoder: Decoder[F, S, A, O],
    bEncoder: Encoder[F, S, B, E],
    bDecoder: Decoder[F, S, B, O],
    eqFOptA: Eq[F[Option[A]]],
    eqFListA: Eq[F[List[A]]],
    optEncoder: Encoder[F, S, Option[A], E],
    optDecoder: Decoder[F, S, Option[A], O],
    eqFChainA: Eq[F[Chain[A]]],
    eqFNelA: Eq[F[NonEmptyList[A]]],
    eqFNevA: Eq[F[NonEmptyVector[A]]],
    eqFNeSetA: Eq[F[NonEmptySet[A]]],
    eqFNeStreamA: Eq[F[NonEmptyStream[A]]],
    eqFNecA: Eq[F[NonEmptyChain[A]]],
    chainEncoder: Encoder[F, S, Chain[A], E],
    chainDecoder: Decoder[F, S, Chain[A], O],
    nelEncoder: Encoder[F, S, NonEmptyList[A], E],
    nelDecoder: Decoder[F, S, NonEmptyList[A], O],
    nevEncoder: Encoder[F, S, NonEmptyVector[A], E],
    nevDecoder: Decoder[F, S, NonEmptyVector[A], O],
    neSetEncoder: Encoder[F, S, NonEmptySet[A], E],
    neSetDecoder: Decoder[F, S, NonEmptySet[A], O],
    neStreamEncoder: Encoder[F, S, NonEmptyStream[A], E],
    neStreamDecoder: Decoder[F, S, NonEmptyStream[A], O],
    necEncoder: Encoder[F, S, NonEmptyChain[A], E],
    necDecoder: Decoder[F, S, NonEmptyChain[A], O],
    listDecoder: Decoder[F, S, List[A], O]
  ): RuleSet = new RuleSet {
    override def name: String = "derivedEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(encodeDecode[A])
    override def props: Seq[(String, Prop)] = Seq(
      "option encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Option[A]] _),
      "chain encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Chain[A]] _),
      "non-empty list encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyList[A]] _),
      "non-empty vector encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyVector[A]] _),
      "non-empty set encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptySet[A]] _),
      "non-empty stream encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyStream[A]] _),
      "non-empty chain encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyChain[A]] _),
      "either eitherLeftEncodeDecode " -> forAll(laws.eitherLeftEncodeDecode[B, A] _),
      "either eitherRightEncodeDecode " -> forAll(laws.eitherRightEncodeDecode[B, A] _),
      "either eitherLeftDefaultDecode " -> forAll(laws.eitherLeftDefaultDecode[B, A] _),
      "either eitherRightDefaultDecode " -> forAll(laws.eitherRightDefaultDecode[B, A] _)
    )
  }
}

object EncoderDecoderDerivedTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](settings: S)(
    implicit fin: Transform[F, S, E, D],
    prep: Transform[F, S, D, O],
    hv: HasValue[F, S, O]
  ): EncoderDecoderDerivedTests[F, S, E, D, O] =
    new EncoderDecoderDerivedTests[F, S, E, D, O] {
      override def laws: EncoderDecoderDerivedLaws[F, S, E, D, O] = EncoderDecoderDerivedLaws[F, S, E, D, O](settings)
    }
}
