package extruder.core

import java.net.URL

import cats.Eq
import cats.syntax.either._
import cats.instances.all._
import cats.kernel.laws.discipline.MonoidTests
import extruder.core.TestCommon._
import extruder.core.ValidationCatsInstances._
import extruder.effect.ExtruderMonadError
import org.scalacheck.Gen.Choose
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

import scala.concurrent.duration.{Duration, FiniteDuration}

trait SourceSpec extends Specification with ScalaCheck with EitherMatchers with Discipline {
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

  type Eff[F[_]] = ExtruderMonadError[F]
  override type OutputData = InputData

  val supportsEmptyNamespace: Boolean = true
  def ext: SpecStructure = s2""""""
  def monoidTests: MonoidTests[EncodeData]#RuleSet
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

      Can represent the following types as a table of required params

      ${checkAll("Encoder monoid", monoidTests)}
      $ext
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
      testList(Gen.listOf(gen).suchThat(_.nonEmpty))

  def testList[T, F[T] <: TraversableOnce[T]](
    gen: Gen[F[T]]
  )(implicit encoder: Enc[Validation, F[T]], decoder: Dec[Validation, F[T]]): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      (for {
        encoded <- encode[F[T]](namespace, value)
        decoded <- decode[F[T]](namespace, encoded)
      } yield decoded) must beRight.which(_.toList === value.filter(_.toString.trim.nonEmpty).toList)
    }

  def test[T](
    gen: Gen[T]
  )(implicit encoder: Enc[Validation, T], decoder: Dec[Validation, T], teq: Eq[T], equals: Eq[Validation[T]]): Prop = {
    val F: Eff[Validation] = ExtruderMonadError[Validation]
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      def eqv(encoded: Validation[OutputData], decoded: InputData => Validation[T]): Boolean =
        equals.eqv(F.flatMap(encoded)(decoded), Right(value))

      eqv(encode[T](namespace, value), decode[T](namespace, _)) &&
      (if (supportsEmptyNamespace) eqv(encode[T](value), decode[T]) else true)
    }
  }

  def testDefaults: Prop =
    Prop.forAll(Gen.alphaNumStr, Gen.posNum[Int], Gen.posNum[Long])(
      (s, i, l) =>
        decode[CaseClass](
          convertData(
            Map(List("CaseClass", "s") -> s, List("CaseClass", "i") -> i.toString, List("CaseClass", "l") -> l.toString)
          )
        ) must beRight(CaseClass(s, i, l, None))
    )

  def testDefaultDecode(implicit cvEq: Eq[Validation[CaseClass]]): Boolean =
    cvEq.eqv(decode[CaseClass], Right(expectedCaseClass)) &&
      cvEq.eqv(decode[CaseClass](List.empty), Right(expectedCaseClass))

  def testCaseClassParams(implicit hints: Hint): MatchResult[String] = parameters[CaseClass] !== ""
}
