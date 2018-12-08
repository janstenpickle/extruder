package extruder.laws

import cats.data.NonEmptyList
import cats.{Eq, Monad, Monoid}
import extruder.core.{DecoderT, EncoderT, ExtruderErrors, Settings}
import extruder.data.Transform
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws
import org.scalacheck.ScalacheckShapeless._

trait EncoderDecoderGenericTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderMapTests[F, S, E, D, O] {
  def genericEncodeDecode[A: Arbitrary, B: Arbitrary](
    implicit eqFA: Eq[F[A]],
    encoder: EncoderT[F, S, A, E],
    decoder: DecoderT[F, S, A, O],
    eqFOptA: Eq[F[Option[A]]],
    optEncoder: EncoderT[F, S, Option[A], E],
    optDecoder: DecoderT[F, S, Option[A], O],
    eqFNelA: Eq[F[NonEmptyList[A]]],
    nelEncoder: EncoderT[F, S, NonEmptyList[A], E],
    nelDecoder: DecoderT[F, S, NonEmptyList[A], O],
    multiShowEncoder: EncoderT[F, S, (A, B), E],
    multiShowDecoder: DecoderT[F, S, (A, B), O],
    eqFTuple: Eq[F[(A, B)]],
    optMultiShowEncoder: EncoderT[F, S, Option[(A, B)], E],
    optMultiShowDecoder: DecoderT[F, S, Option[(A, B)], O],
    optEqFTuple: Eq[F[Option[(A, B)]]],
    mapEncoder: EncoderT[F, S, Map[B, A], E],
    mapDecoder: DecoderT[F, S, Map[B, A], O],
    eqFMapBA: Eq[F[Map[B, A]]],
    caseClassEncoder: EncoderT[F, S, CaseClass, E],
    caseClassDecoder: DecoderT[F, S, CaseClass, O],
    eqFCaseClass: Eq[F[CaseClass]]
  ): RuleSet = new RuleSet {
    override def name: String = "mapEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(mapEncodeDecode[A, B])
    override def props: Seq[(String, Prop)] =
      Seq(
        "generic encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[CaseClass] _),
        "generic encodeDecodeWithPartiallyApplied" -> forAll(laws.encodeDecodeWithPartiallyApplied[CaseClass] _)
      )
  }
}

object EncoderDecoderGenericTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](
    settings: S
  )(implicit fin: Transform[F, S, E, D], prep: Transform[F, S, D, O]): EncoderDecoderGenericTests[F, S, E, D, O] =
    new EncoderDecoderGenericTests[F, S, E, D, O] {
      override def laws: EncoderDecoderLaws[F, S, E, D, O] = EncoderDecoderLaws[F, S, E, D, O](settings)
    }
}
