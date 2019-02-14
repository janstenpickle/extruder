package extruder.laws

import cats.{Eq, Monad, Monoid}
import extruder.core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait EncoderDecoderTests[F[_], S <: Settings, E, D, O] extends DecoderTests[F, S, E, D, O] {
  def laws: EncoderDecoderLaws[F, S, E, D, O]

  def encodeDecode[A: Arbitrary](
    implicit eqFa: Eq[F[A]],
    eqFOptA: Eq[F[Option[A]]],
    eqFListA: Eq[F[List[A]]],
    encoder: EncoderT[F, S, A, E],
    decoder: DecoderT[F, S, A, O],
    optDecoder: DecoderT[F, S, Option[A], O],
    listDecoder: DecoderT[F, S, List[A], O]
  ): RuleSet = new RuleSet {
    override def name: String = "encoderDecoder"
    override def bases: Seq[(String, Laws#RuleSet)] = Nil
    override def parents: Seq[RuleSet] = Seq(decode[A])
    override def props: Seq[(String, Prop)] = Seq(
      "encoderDecoder encodeFinalizePrepareDecode" -> forAll(laws.encodeFinalizePrepareDecode[A] _),
      "encoderDecoder encodeFailFinalizePrepareDecode" -> forAll(laws.encodeFailFinalizePrepareDecode[A] _),
      "encoderDecoder encodeFinalizeFailPrepareDecode" -> forAll(laws.encodeFinalizeFailPrepareDecode[A] _),
      "encoderDecoder encodeFinalizePrepareFailDecode" -> forAll(laws.encodeFinalizePrepareFailDecode[A] _),
      "encoderDecoder encodeFinalizePrepareDecodeFail" -> forAll(laws.encodeFinalizePrepareDecodeFail[A] _),
      "encoderDecoder encodeDecodeWithPartiallyApplied" -> forAll(laws.encodeDecodeWithPartiallyApplied[A] _),
      "encoderDecoder combinedEncodeDecodeWithPartiallyApplied" -> forAll(
        laws.combinedEncodeDecodeWithPartiallyApplied[A] _
      ),
      "encoderDecoder combinedEncodeDecodeWithPartiallyAppliedLeftFail" -> forAll(
        laws.combinedEncodeDecodeWithPartiallyAppliedLeftFail[A] _
      ),
      "encoderDecoder combinedEncodeDecodeOption" -> forAll(laws.combinedEncodeDecodeOption[A] _),
      "encoderDecoder combinedEncodeDecodeOptionLeftFail" -> forAll(laws.combinedEncodeDecodeOptionLeftFail[A] _)
    )
  }
}

object EncoderDecoderTests {
  def apply[F[_]: Monad: ExtruderErrors, S <: Settings, E: Monoid, D, O: Monoid](
    settings: S
  )(implicit fin: Transform[F, S, E, D], prep: Transform[F, S, D, O]): EncoderDecoderTests[F, S, E, D, O] =
    new EncoderDecoderTests[F, S, E, D, O] {
      override def laws: EncoderDecoderLaws[F, S, E, D, O] = EncoderDecoderLaws[F, S, E, D, O](settings)
    }
}
