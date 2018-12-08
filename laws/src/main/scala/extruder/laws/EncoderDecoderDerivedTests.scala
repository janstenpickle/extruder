package extruder.laws

import cats.data.NonEmptyList
import cats.{Eq, Monad, Monoid}
import extruder.core.{DecoderT, EncoderT, ExtruderErrors, Settings}
import extruder.data.Transform
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderDerivedTests[F[_], S <: Settings, E, D, O] extends EncoderDecoderTests[F, S, E, D, O] {
  def derivedEncodeDecode[A: Arbitrary](
    implicit eqFA: Eq[F[A]],
    encoder: EncoderT[F, S, A, E],
    decoder: DecoderT[F, S, A, O],
    eqFOptA: Eq[F[Option[A]]],
    optEncoder: EncoderT[F, S, Option[A], E],
    optDecoder: DecoderT[F, S, Option[A], O],
    eqFNelA: Eq[F[NonEmptyList[A]]],
    nelEncoder: EncoderT[F, S, NonEmptyList[A], E],
    nelDecoder: DecoderT[F, S, NonEmptyList[A], O]
  ): RuleSet = new RuleSet {
    override def name: String = "derivedEncodeDecode"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(encodeDecode[A])
    override def props: Seq[(String, Prop)] = Seq(
      "option encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[Option[A]] _),
      "non-empty list encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[NonEmptyList[A]] _),
      "option encodeDecodeWithPartiallyApplied" -> forAll(laws.encodeDecodeWithPartiallyApplied[Option[A]] _),
      "non-empty list encodeDecodeWithPartiallyApplied" -> forAll(
        laws.encodeDecodeWithPartiallyApplied[NonEmptyList[A]] _
      )
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
