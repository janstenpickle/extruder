package extruder.core

import java.net.URL

import cats.effect.IO
import cats.instances.all._
import cats.kernel.laws.GroupLaws
import cats.syntax.all._
import cats.{Eq, FlatMap}
import extruder.core.ConfigValidationCatsInstances._
import extruder.instances.ConfigValidationInstances
import org.scalacheck.Gen.Choose
import org.scalacheck.Shapeless._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import TestCommon._

import scala.util.Try

trait ConfigSpec
    extends Specification
    with ScalaCheck
    with EitherMatchers
    with Discipline
    with ConfigValidationInstances {
  self: Encode
    with Encoders
    with PrimitiveEncoders
    with DerivedEncoders
    with EncodeTypes
    with Decode
    with Decoders
    with DecodeFromDefaultConfig
    with PrimitiveDecoders
    with DerivedDecoders
    with DecodeTypes =>

  override type OutputConfig = InputConfig

  val supportsEmptyNamespace: Boolean = true
  def ext: SpecStructure = s2""""""
  def monoidGroupLaws: GroupLaws[EncodeConfig]
  implicit def utils: Hint

  implicit val caseClassEq: Eq[CaseClass] = Eq.fromUniversalEquals
  implicit val urlEq: Eq[URL] = Eq.fromUniversalEquals
  implicit val durationEq: Eq[Duration] = Eq.fromUniversalEquals
  implicit val finiteDurationEq: Eq[FiniteDuration] = Eq.fromUniversalEquals

  val caseClassConfig: Map[Seq[String], String] =
    Map(Seq("CaseClass", "s") -> "string", Seq("CaseClass", "i") -> "1", Seq("CaseClass", "l") -> "1")

  val expectedCaseClass = CaseClass("string", 1, 1L, None)

  def convertConfig(map: Map[Seq[String], String])(implicit utils: Hint): InputConfig

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

      Can load config defaults with
        Standard sync decode $testDefaultDecode
        IO async decode $testDefaultDecodeIO
        Future async decode $testDefaultDecodeAsync
        Unsafe decode $testDefaultDecodeUnsafe

      Can represent the following types as a table of required params
        Case class tree $testCaseClassParams
      $ext

      ${checkAll("Encoder monoid", monoidGroupLaws.monoid(monoid))}
      """

  def testNumeric[T: Numeric](
    implicit encoder: Enc[ConfigValidation, T],
    decoder: Dec[ConfigValidation, T],
    eitherEncoder: Enc[EitherErrors, T],
    eitherDecoder: Dec[EitherErrors, T],
    listEncoder: Enc[ConfigValidation, List[T]],
    listDecoder: Dec[ConfigValidation, List[T]],
    tEq: Eq[T],
    choose: Choose[T]
  ): Prop =
    testType(Gen.posNum[T]) ++
      testType(Gen.negNum[T])

  def testType[T](gen: Gen[T])(
    implicit encoder: Enc[ConfigValidation, T],
    decoder: Dec[ConfigValidation, T],
    eitherEncoder: Enc[EitherErrors, T],
    eitherDecoder: Dec[EitherErrors, T],
    listEncoder: Enc[ConfigValidation, List[T]],
    listDecoder: Dec[ConfigValidation, List[T]],
    tEq: Eq[T]
  ): Prop =
    test(gen) ++
      test(Gen.option(gen)) ++
      testList(Gen.listOf(gen).suchThat(_.nonEmpty)) ++
      testIO[T](gen) ++
      testAsync[T](gen) ++
      testUnsafe[T](gen)

  def testList[T, F[T] <: TraversableOnce[T]](
    gen: Gen[F[T]]
  )(implicit encoder: Enc[ConfigValidation, F[T]], decoder: Dec[ConfigValidation, F[T]]): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      (for {
        encoded <- encode[F[T]](namespace, value).toEither
        decoded <- decode[F[T]](namespace, encoded).toEither
      } yield decoded) must beRight.which(_.toList === value.filter(_.toString.trim.nonEmpty).toList)
    }

  def test[T](gen: Gen[T])(
    implicit encoder: Enc[ConfigValidation, T],
    decoder: Dec[ConfigValidation, T],
    teq: Eq[T],
    equals: Eq[ConfigValidation[T]],
    FM: FlatMap[ConfigValidation]
  ): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      def eqv(encoded: ConfigValidation[OutputConfig], decoded: InputConfig => ConfigValidation[T]): Boolean =
        equals.eqv(FM.flatMap(encoded)(decoded), value.validNel)

      eqv(encode[T](namespace, value), decode[T](namespace, _)) &&
      (if (supportsEmptyNamespace) eqv(encode[T](value), decode[T]) else true)
    }

  def testIO[T](gen: Gen[T])(
    implicit encoder: Enc[ConfigValidation, T],
    decoder: Dec[ConfigValidation, T],
    teq: Eq[T],
    equals: Eq[IO[ConfigValidation[T]]]
  ): Prop = Prop.forAll(gen, namespaceGen) { (value, namespace) =>
    def eqv(encoded: IO[ConfigValidation[OutputConfig]], decoded: InputConfig => IO[ConfigValidation[T]]): Boolean =
      equals.eqv(ioFlatMapForConfigValidation.flatMap(encoded)(decoded), IO(value.validNel))

    eqv(encodeIO[T](namespace, value), decodeIO[T](namespace, _)) &&
    (if (supportsEmptyNamespace) eqv(encodeIO[T](value), decodeIO[T]) else true)
  }

  def testAsync[T](gen: Gen[T])(
    implicit encoder: Enc[ConfigValidation, T],
    decoder: Dec[ConfigValidation, T],
    teq: Eq[T],
    equals: Eq[IO[ConfigValidation[T]]],
    ioc: IOConvert[Future]
  ): Prop = Prop.forAll(gen, namespaceGen) { (value, namespace) =>
    def eqv(
      encoded: Future[ConfigValidation[OutputConfig]],
      decoded: InputConfig => Future[ConfigValidation[T]]
    ): Boolean =
      equals
        .eqv(ioFlatMapForConfigValidation.flatMap(ioc.toIO(encoded))(e => ioc.toIO(decoded(e))), IO(value.validNel))

    eqv(encodeAsync[T, Future](namespace, value), decodeAsync[T, Future](namespace, _)) &&
    (if (supportsEmptyNamespace) eqv(encodeAsync[T, Future](value), decodeAsync[T, Future]) else true)
  }

  def testUnsafe[T](gen: Gen[T])(
    implicit encoder: Enc[ConfigValidation, T],
    decoder: Dec[ConfigValidation, T],
    teq: Eq[T],
    equals: Eq[Try[T]],
    FM: FlatMap[Try]
  ): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      def eqv(encoded: Try[OutputConfig], decoded: InputConfig => Try[T]): Boolean =
        equals.eqv(FM.flatMap(encoded)(decoded), Try(value))

      eqv(Try(encodeUnsafe[T](namespace, value)), v => Try(decodeUnsafe[T](namespace, v))) &&
      (if (supportsEmptyNamespace) eqv(Try(encodeUnsafe[T](value)), v => Try(decodeUnsafe[T](v))) else true)
    }

  def testDefaults: Prop =
    Prop.forAll(Gen.alphaNumStr, Gen.posNum[Int], Gen.posNum[Long])(
      (s, i, l) =>
        decode[CaseClass](
          convertConfig(
            Map(Seq("CaseClass", "s") -> s, Seq("CaseClass", "i") -> i.toString, Seq("CaseClass", "l") -> l.toString)
          )
        ).toEither must beRight(CaseClass(s, i, l, None))
    )

  def testDefaultDecode(implicit cvEq: Eq[ConfigValidation[CaseClass]]): Boolean =
    cvEq.eqv(decode[CaseClass], expectedCaseClass.validNel) &&
      cvEq.eqv(decode[CaseClass](Seq.empty), expectedCaseClass.validNel)

  def testDefaultDecodeIO(implicit ioEq: Eq[IO[ConfigValidation[CaseClass]]]): Boolean =
    ioEq.eqv(decodeIO[CaseClass], IO(expectedCaseClass.validNel)) &&
      ioEq.eqv(decodeIO[CaseClass](Seq.empty), IO(expectedCaseClass.validNel))

  def testDefaultDecodeAsync(implicit futEq: Eq[Future[ConfigValidation[CaseClass]]]): Boolean =
    futEq.eqv(decodeAsync[CaseClass, Future], Future(expectedCaseClass.validNel)) &&
      futEq.eqv(decodeAsync[CaseClass, Future](Seq.empty), Future(expectedCaseClass.validNel))

  def testDefaultDecodeUnsafe(implicit ccEq: Eq[CaseClass]): Boolean =
    ccEq.eqv(decodeUnsafe[CaseClass], expectedCaseClass) &&
      ccEq.eqv(decodeUnsafe[CaseClass](Seq.empty), expectedCaseClass)

  def testCaseClassParams(implicit utils: Hint): MatchResult[String] = parameters[CaseClass] !== ""
}
