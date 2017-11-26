package extruder.core

import java.net.URL

import cats.instances.all._
import cats.kernel.laws.GroupLaws
import cats.syntax.all._
import cats.{Eq, FlatMap}
import extruder.core.ValidationCatsInstances._
import extruder.instances.ValidationInstances
import org.scalacheck.Gen.Choose
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

import scala.concurrent.duration.{Duration, FiniteDuration}
import TestCommon._
import extruder.effect.ExtruderAsync

import scala.util.Try

trait SourceSpec extends Specification with ScalaCheck with EitherMatchers with Discipline with ValidationInstances {
  self: Encode
    with Encoders
    with PrimitiveEncoders
    with DerivedEncoders
    with EncodeTypes
    with Decode
    with Decoders
    with DecodeFromDefaultSource
    with PrimitiveDecoders
    with DerivedDecoders
    with DecodeTypes =>

  override type OutputData = InputData

  val supportsEmptyNamespace: Boolean = true
  def ext: SpecStructure = s2""""""
  def monoidGroupLaws: GroupLaws[EncodeData]
  implicit def hints: Hint

  implicit val caseClassEq: Eq[CaseClass] = Eq.fromUniversalEquals
  implicit val urlEq: Eq[URL] = Eq.fromUniversalEquals
  implicit val durationEq: Eq[Duration] = Eq.fromUniversalEquals
  implicit val finiteDurationEq: Eq[FiniteDuration] = Eq.fromUniversalEquals

  val caseClassData: Map[List[String], String] =
    Map(List("CaseClass", "s") -> "string", List("CaseClass", "i") -> "1", List("CaseClass", "l") -> "1")

  val expectedCaseClass = CaseClass("string", 1, 1L, None)

  def convertData(map: Map[List[String], String])(implicit hints: Hint): InputData

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

      Can load data defaults with
        Standard sync decode $testDefaultDecode
        Unsafe decode $testDefaultDecodeUnsafe

      Can represent the following types as a table of required params
        Case class tree $testCaseClassParams
      $ext

      ${checkAll("Encoder monoid", monoidGroupLaws.monoid(monoid))}
      """

  def testNumeric[T: Numeric](
    implicit encoder: Enc[Validation, T],
    decoder: Dec[Validation, T],
    listEncoder: Enc[Validation, List[T]],
    listDecoder: Dec[Validation, List[T]],
    tEq: Eq[T],
    choose: Choose[T]
  ): Prop =
    testType(Gen.posNum[T]) ++
      testType(Gen.negNum[T])

  def testType[T](gen: Gen[T])(
    implicit encoder: Enc[Validation, T],
    decoder: Dec[Validation, T],
    listEncoder: Enc[Validation, List[T]],
    listDecoder: Dec[Validation, List[T]],
    tEq: Eq[T]
  ): Prop =
    test(gen) ++
      test(Gen.option(gen)) ++
      testList(Gen.listOf(gen).suchThat(_.nonEmpty)) ++
      testUnsafe[T](gen)

  def testList[T, F[T] <: TraversableOnce[T]](
    gen: Gen[F[T]]
  )(implicit encoder: Enc[Validation, F[T]], decoder: Dec[Validation, F[T]]): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      (for {
        encoded <- encode[F[T]](namespace, value).toEither
        decoded <- decode[F[T]](namespace, encoded).toEither
      } yield decoded) must beRight.which(_.toList === value.filter(_.toString.trim.nonEmpty).toList)
    }

  def test[T](
    gen: Gen[T]
  )(implicit encoder: Enc[Validation, T], decoder: Dec[Validation, T], teq: Eq[T], equals: Eq[Validation[T]]): Prop = {
    val F: ExtruderAsync[Validation] = ExtruderAsync[Validation]
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      def eqv(encoded: Validation[OutputData], decoded: InputData => Validation[T]): Boolean =
        equals.eqv(F.flatMap(encoded)(decoded), value.validNel)

      eqv(encode[T](namespace, value), decode[T](namespace, _)) &&
      (if (supportsEmptyNamespace) eqv(encode[T](value), decode[T]) else true)
    }
  }

  def testUnsafe[T](gen: Gen[T])(
    implicit encoder: Enc[Validation, T],
    decoder: Dec[Validation, T],
    teq: Eq[T],
    equals: Eq[Try[T]],
    FM: FlatMap[Try]
  ): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      def eqv(encoded: Try[OutputData], decoded: InputData => Try[T]): Boolean =
        equals.eqv(FM.flatMap(encoded)(decoded), Try(value))

      eqv(Try(encodeUnsafe[T](namespace, value)), v => Try(decodeUnsafe[T](namespace, v))) &&
      (if (supportsEmptyNamespace) eqv(Try(encodeUnsafe[T](value)), v => Try(decodeUnsafe[T](v))) else true)
    }

  def testDefaults: Prop =
    Prop.forAll(Gen.alphaNumStr, Gen.posNum[Int], Gen.posNum[Long])(
      (s, i, l) =>
        decode[CaseClass](
          convertData(
            Map(
              List("CaseClass", "s") -> s,
              List("CaseClass", "i") -> i.toString,
              List("CaseClass", "l") -> l.toString
            )
          )
        ).toEither must beRight(CaseClass(s, i, l, None))
    )

  def testDefaultDecode(implicit cvEq: Eq[Validation[CaseClass]]): Boolean =
    cvEq.eqv(decode[CaseClass], expectedCaseClass.validNel) &&
      cvEq.eqv(decode[CaseClass](List.empty), expectedCaseClass.validNel)

  def testDefaultDecodeUnsafe(implicit ccEq: Eq[CaseClass]): Boolean =
    ccEq.eqv(decodeUnsafe[CaseClass], expectedCaseClass) &&
      ccEq.eqv(decodeUnsafe[CaseClass](List.empty), expectedCaseClass)

  def testCaseClassParams(implicit hints: Hint): MatchResult[String] = parameters[CaseClass] !== ""
}
