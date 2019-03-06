package extruder.laws

import cats.data._
import cats.{Eq, Monad, Monoid}
import extruder.core._
import extruder.data.PathElement
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderMultiTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderDerivedTests[F, S, E, D, O] {
  implicit def F: Monad[F]
  implicit def errors: ExtruderErrors[F]
  implicit def monoid: Monoid[E]

  def multiEncodeDecode[A: Arbitrary: Parser: Show, B: Arbitrary: Parser: Show, C: Arbitrary](
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
    chainEncoder: Encoder[F, S, Chain[A], E],
    chainDecoder: Decoder[F, S, Chain[A], O],
    nelEncoder: Encoder[F, S, NonEmptyList[A], E],
    nelDecoder: Decoder[F, S, NonEmptyList[A], O],
    nevEncoder: Encoder[F, S, NonEmptyVector[A], E],
    nevDecoder: Decoder[F, S, NonEmptyVector[A], O],
    necEncoder: Encoder[F, S, NonEmptyChain[A], E],
    necDecoder: Decoder[F, S, NonEmptyChain[A], O],
    listDecoder: Decoder[F, S, List[A], O],
    eqFMapBA: Eq[F[Map[B, A]]],
    optStringDecoder: Decoder[F, S, Option[String], O],
    stringEncoder: Encoder[F, S, String, E],
    eqFMultiClass: Eq[F[MultiClass[A, B]]],
    optEqFMultiClass: Eq[F[Option[MultiClass[A, B]]]]
  ): RuleSet = new RuleSet {
    implicit val stringReader: StringReader[F, S, O] = new StringReader[F, S, O] {
      override def lookup(path: List[PathElement], settings: S, data: O): F[Option[String]] =
        optStringDecoder.read(path, settings, None, data)
    }
    implicit val stringWriter: StringWriter[F, S, E] = new StringWriter[F, S, E] {
      override def write(path: List[PathElement], settings: S, value: String): F[E] =
        stringEncoder.write(path, settings, value)
    }

    implicit val multiParser: MultiParser[F, MultiClass[A, B]] = new MultiParser[F, MultiClass[A, B]] {
      override def parse(
        lookup: List[String] => OptionT[F, String]
      ): OptionT[F, ValidatedNel[String, MultiClass[A, B]]] =
        for {
          a <- lookup(List("a"))
          b <- lookup(List("b"))
        } yield Parser[A].parseNel(a).product(Parser[B].parseNel(b)).map { case (a0, b0) => new MultiClass(a0, b0) }
    }

    implicit val multiShow: MultiShow[MultiClass[A, B]] = new MultiShow[MultiClass[A, B]] {
      override def show(value: MultiClass[A, B]): Map[List[String], String] =
        Map(List("a") -> Show[A].show(value.a), List("b") -> Show[B].show(value.b))
    }

    Decoder.multiParserDecode[F, MultiClass[A, B], S, O]

    override def name: String = "multiEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(derivedEncodeDecode[A, C])
    override def props: Seq[(String, Prop)] =
      Seq(
        "tuple encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[MultiClass[A, B]] _),
        "tuple encodeDecodeWithPartiallyApplied" -> forAll(laws.encodeDecodeWithPartiallyApplied[MultiClass[A, B]] _),
        "optional tuple encodeFinalizePrepareDecode" -> forAll(
          laws.encodeFinalizePrepareDecode[Option[MultiClass[A, B]]] _
        ),
        "optional tuple encodeDecodeWithPartiallyApplied" -> forAll(
          laws.encodeDecodeWithPartiallyApplied[Option[MultiClass[A, B]]] _
        )
      )
  }
}

object EncoderDecoderMultiTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](settings: S)(
    implicit fin: Transform[F, S, E, D],
    prep: Transform[F, S, D, O],
    hv: HasValue[F, S, O]
  ): EncoderDecoderMultiTests[F, S, E, D, O] =
    new EncoderDecoderMultiTests[F, S, E, D, O] {
      override def F: Monad[F] = Monad[F]
      override def monoid: Monoid[E] = Monoid[E]
      override def errors: ExtruderErrors[F] = ExtruderErrors[F]
      override def laws: EncoderDecoderDerivedLaws[F, S, E, D, O] = EncoderDecoderDerivedLaws[F, S, E, D, O](settings)
    }
}
