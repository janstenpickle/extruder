package extruder.core

import cats.kernel.laws.GroupLaws
import cats.syntax.either._
import org.scalacheck.Gen.Choose
import org.scalacheck.Shapeless._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

trait ConfigSpec[C, I, J, D[T] <: Decoder[T, I], E[T] <: Encoder[T, J]] extends Specification with ScalaCheck with EitherMatchers with Discipline {
  self: Encode[J, C, E] with
        Encoders[J, E] with
        PrimitiveEncoders[J, E] with
        DerivedEncoders[J, E] with
        Decode[C, I, D] with
        Decoders[I, D] with
        PrimitiveDecoders[I, D] with
        DerivedDecoders[I, D] =>

  import TestCommon._

  def ext: SpecStructure = s2""""""
  def monoidGroupLaws: GroupLaws[J]

  override def is: SpecStructure =
    s2"""
       Can decode and encode the following types
        String ${testType(Gen.alphaNumStr)}
        Int ${testNumeric[Int]}
        Long ${testNumeric[Long]}
        Double ${testNumeric[Double]}
        Float ${testNumeric[Float]}
        Short ${testNumeric[Short]}
        Byte ${testNumeric[Byte]}
        Boolean ${testType(Gen.oneOf(true, false))}
        URL ${testType(urlGen)}
        Duration ${testType(durationGen)}
        FiniteDuration ${testType(finiteDurationGen)}
        Case class tree ${test(Gen.resultOf(CaseClass))}
        Case class with defaults set $testDefaults

      Can represent the following types as a table of required params
        Case class tree $testCaseClassParams
      $ext

      ${checkAll("Encoder monoid", monoidGroupLaws.monoid(monoid))}
      """

  def testNumeric[T: Numeric](implicit encoder: E[T],
                              decoder: D[T],
                              optEncoder: E[Option[T]],
                              optDecoder: D[Option[T]],
                              listEncoder: E[List[T]],
                              listDecoder: D[List[T]],
                              choose: Choose[T]): Prop =
      testType(Gen.posNum[T]) ++ testType(Gen.negNum[T])

  def testType[T](gen: Gen[T])(implicit encoder: E[T],
                               decoder: D[T],
                               optEncoder: E[Option[T]],
                               optDecoder: D[Option[T]],
                               listEncoder: E[List[T]],
                               listDecoder: D[List[T]]): Prop =
    test(gen) ++ test(Gen.option(gen)) ++ testList(Gen.listOf(gen).suchThat(_.nonEmpty))

  def testList[T, F[T] <: TraversableOnce[T]](gen: Gen[F[T]])(implicit encoder: E[F[T]], decoder: D[F[T]]): Prop =
    Prop.forAllNoShrink(gen, namespaceGen) { (value, namespace) =>
      (for {
        encoded <- encode[F[T]](namespace, value).toEither
        decoded <- decode[F[T]](namespace, encoded).toEither
      } yield decoded) must beRight.which(_.toList === value.filter(_.toString.trim.nonEmpty).toList)
    }

  def test[T](gen: Gen[T])(implicit encoder: E[T], decoder: D[T]): Prop =
    Prop.forAllNoShrink(gen, namespaceGen) { (value, namespace) =>
      (for {
        encoded <- encode[T](namespace, value).toEither
        decoded <- decode[T](namespace, encoded).toEither
      } yield decoded) must beRight(value)
    }

  def testDefaults(implicit decoder: D[CaseClass]): Prop =
    Prop.forAll(Gen.alphaNumStr, Gen.posNum[Int], Gen.posNum[Long])((s, i, l) =>
      decode[CaseClass](convertConfig(Map(
        "caseclass.s" -> s,
        "caseclass.i" -> i.toString,
        "caseclass.l" -> l.toString
      ))).toEither must beRight(CaseClass(s, i, l, None))
    )

  def convertConfig(map: Map[String, String]): C

  def testCaseClassParams: MatchResult[String] = parameters[CaseClass] !== ""
}