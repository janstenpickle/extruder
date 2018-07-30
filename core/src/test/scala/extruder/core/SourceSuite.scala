package extruder.core

import java.net.URL

import cats.data.{NonEmptyList, OptionT, ValidatedNel}
import cats.instances.all._
import cats.kernel.laws.discipline.MonoidTests
import cats.{Eq, Monad}
import extruder.core.TestCommon._
import extruder.core.ValidationCatsInstances._
import extruder.effect.ExtruderMonadError
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.Choose
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite}
import org.typelevel.discipline.scalatest.Discipline

import scala.concurrent.duration.{Duration, FiniteDuration}

abstract class SourceSuite[A](monoidTests: MonoidTests[A]#RuleSet)
    extends FunSuite
    with GeneratorDrivenPropertyChecks
    with EitherValues
    with Discipline {
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

  implicit val caseClassEq: Eq[CaseClass] = Eq.fromUniversalEquals
  implicit val urlEq: Eq[URL] = Eq.fromUniversalEquals
  implicit val durationEq: Eq[Duration] = Eq.fromUniversalEquals
  implicit val finiteDurationEq: Eq[FiniteDuration] = Eq.fromUniversalEquals

  lazy val caseClassData: Map[List[String], String] =
    Map(List("CaseClass", "s") -> "string", List("CaseClass", "i") -> "1", List("CaseClass", "l") -> "1").map {
      case (k, v) =>
        if (defaultSettings.includeClassNameInPath) k -> v
        else k.filterNot(_ == "CaseClass") -> v
    }

  val expectedCaseClass = CaseClass("string", 1, 1L, None)

  def convertData(map: Map[List[String], String]): InputData

  checkAll("Encoder monoid", monoidTests)
  test("Can decode and encode a String") { testType(Gen.alphaNumStr.suchThat(_.nonEmpty)) }
  test("Can decode and encode an Int") { testNumeric[Int] }
  test("Can decode and encode a Long") { testNumeric[Long] }
  test("Can decode and encode a Double") { testNumeric[Double] }
  test("Can decode and encode a Float") { testNumeric[Float] }
  test("Can decode and encode a Short") { testNumeric[Short] }
  test("Can decode and encode a Byte") { testNumeric[Byte] }
  test("Can decode and encode a Boolean") { testType(Gen.oneOf(true, false)) }
//  test("Can decode and encode a URL") { testType(urlGen) }
  test("Can decode and encode a Duration") { testType(durationGen) }
  test("Can decode and encode a FiniteDuration") { testType(finiteDurationGen) }
  test("Can decode and encode a Case class tree") { test(Gen.resultOf(CaseClass)) }
  test("Can decode and encode a Case class with defaults set") { testDefaults }
  test("Can decode and encode a Tuple") { test(implicitly[Arbitrary[(Int, Long)]].arbitrary) }
  test("Can decode and encode a Option Tuple") { test(Gen.option(implicitly[Arbitrary[(Int, Long)]].arbitrary)) }
  test("Can load data defaults with standard sync decode") { testDefaultDecode }

  def testNumeric[T: Numeric](
    implicit encoder: Enc[Validation, T],
    decoder: Dec[Validation, T],
    listEncoder: Enc[Validation, List[T]],
    listDecoder: Dec[Validation, List[T]],
    tEq: Eq[T],
    choose: Choose[T]
  ): Assertion = {
    testType(Gen.posNum[T])
    testType(Gen.negNum[T])
  }

  def testType[T](gen: Gen[T])(
    implicit encoder: Enc[Validation, T],
    decoder: Dec[Validation, T],
    listEncoder: Enc[Validation, List[T]],
    listDecoder: Dec[Validation, List[T]],
    tEq: Eq[T]
  ): Assertion = {
    test(gen)
    test(Gen.option(gen))
    testList(Gen.listOf(gen).suchThat(_.nonEmpty))
    testNonEmptyList(gen)
  }

  def testList[T, F[T] <: TraversableOnce[T]](
    gen: Gen[F[T]]
  )(implicit encoder: Enc[Validation, F[T]], decoder: Dec[Validation, F[T]]): Assertion =
    forAll(gen, namespaceGen) { (value, namespace) =>
      assert((for {
        encoded <- encode[F[T]](namespace, value)
        decoded <- decode[F[T]](namespace, encoded)
      } yield decoded).map(_.toList === value.filter(_.toString.trim.nonEmpty).toList).right.value)
    }

  def testNonEmptyList[T](
    gen: Gen[T]
  )(implicit encoder: Enc[Validation, NonEmptyList[T]], decoder: Dec[Validation, NonEmptyList[T]]): Assertion =
    forAll(nonEmptyListGen(gen), namespaceGen) { (value, namespace) =>
      assert((for {
        encoded <- encode[NonEmptyList[T]](namespace, value)
        decoded <- decode[NonEmptyList[T]](namespace, encoded)
      } yield decoded).map(_.toList === value.filter(_.toString.trim.nonEmpty)).right.value)
    }

  def test[T](gen: Gen[T])(
    implicit encoder: Enc[Validation, T],
    decoder: Dec[Validation, T],
    teq: Eq[T],
    equals: Eq[Validation[T]]
  ): Assertion = {
    val F: Eff[Validation] = ExtruderMonadError[Validation]
    forAll(gen, namespaceGen) { (value, namespace) =>
      def eqv(encoded: Validation[OutputData], decoded: InputData => Validation[T]): Boolean =
        equals.eqv(F.flatMap(encoded)(decoded), Right(value))

      assert(eqv(encode[T](namespace, value), decode[T](namespace, _)))
      assert(if (supportsEmptyNamespace) eqv(encode[T](value), decode[T]) else true)
    }
  }

  def testDefaults: Assertion =
    forAll(Gen.alphaNumStr, Gen.posNum[Int], Gen.posNum[Long]) { (s, i, l) =>
      assert(
        decode[CaseClass](
          convertData(
            Map(List("CaseClass", "s") -> s, List("CaseClass", "i") -> i.toString, List("CaseClass", "l") -> l.toString)
              .map {
                case (k, v) =>
                  if (defaultSettings.includeClassNameInPath) k -> v
                  else k.filterNot(_ == "CaseClass") -> v
              }
          )
        ) === Right(CaseClass(s, i, l, None))
      )
    }

  def testDefaultDecode(implicit cvEq: Eq[Validation[CaseClass]]): Assertion = {
    val test1 = cvEq.eqv(decode[CaseClass], Right(expectedCaseClass))
    val test2 = cvEq.eqv(decode[CaseClass](List.empty), Right(expectedCaseClass))
    assert(test1)
    assert(test2)
  }

  def testCaseClassParams: Assertion = assert(parameters[CaseClass] !== "")

  implicit def tuple2Parser[F[_]: Monad, A, B](implicit A: Parser[A], B: Parser[B]): MultiParser[F, (A, B)] =
    new MultiParser[F, (A, B)] {
      override def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, (A, B)]] =
        for {
          _1 <- lookup(List("_1")).map(A.parseNel)
          _2 <- lookup(List("_2")).map(B.parseNel)
        } yield _1.product(_2)
    }

  implicit def tuple2Show[A, B](implicit A: Show[A], B: Show[B]): MultiShow[(A, B)] = new MultiShow[(A, B)] {
    override def show(v: (A, B)): Map[List[String], String] =
      Map(List("_1") -> A.show(v._1), List("_2") -> B.show(v._2))
  }
}
