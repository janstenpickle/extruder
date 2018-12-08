//package extruder.core
//
//import java.net.URL
//
//import cats.data.{NonEmptyList, OptionT, ValidatedNel}
//import cats.instances.all._
//import cats.syntax.functor._
//import cats.syntax.flatMap._
//import cats.kernel.laws.discipline.MonoidTests
//import cats.{Eq, FlatMap, Monad, MonadError}
//import extruder.core.TestCommon._
//import extruder.data.Validation
//import extruder.data.ValidationCatsInstances._
//import org.scalacheck.{Arbitrary, Gen}
//import org.scalacheck.Gen.Choose
//import org.scalacheck.ScalacheckShapeless._
//import org.scalatest.prop.GeneratorDrivenPropertyChecks
//import org.scalatest.{Assertion, EitherValues, FunSuite}
//import org.typelevel.discipline.scalatest.Discipline
//
//import scala.concurrent.duration.{Duration, FiniteDuration}
//
//abstract class SourceSuite[A](monoidTests: MonoidTests[A]#RuleSet)
//    extends FunSuite
//    with GeneratorDrivenPropertyChecks
//    with EitherValues
//    with Discipline {
//  self: Encode
//    with Encoders
//    with ShowEncoders
//    with GenericEncoders
//    with EncodeTypes
//    with Decode
//    with Decoders
//    with DecodeFromDefaultSource
//    with ParserDecoders
//    with GenericDecoders
//    with DecodeTypes =>
//
//  override type DecEff[F[_]] = MonadError[F, Throwable]
//  override type EncEff[F[_]] = MonadError[F, Throwable]
//
//  override type OutputData = InputData
//
//  val supportsEmptyNamespace: Boolean = true
//
//  implicit val caseClassEq: Eq[CaseClass] = Eq.fromUniversalEquals
//  implicit val urlEq: Eq[URL] = Eq.fromUniversalEquals
//  implicit val durationEq: Eq[Duration] = Eq.fromUniversalEquals
//  implicit val finiteDurationEq: Eq[FiniteDuration] = Eq.fromUniversalEquals
//
//  lazy val caseClassData: Map[List[String], String] =
//    Map(List("CaseClass", "s") -> "string", List("CaseClass", "i") -> "1", List("CaseClass", "l") -> "1").map {
//      case (k, v) =>
//        if (defaultSettings.includeClassNameInPath) k -> v
//        else k.filterNot(_ == "CaseClass") -> v
//    }
//
//  val expectedCaseClass = CaseClass("string", 1, 1L, None)
//
//  def convertData(map: Map[List[String], String]): InputData
//
//  checkAll("Encoder monoid", monoidTests)
//  test("Can decode and encode a String") { testType(Gen.alphaNumStr.suchThat(_.nonEmpty)) }
//  test("Can decode and encode a Boolean") { testType(Gen.oneOf(true, false)) }
//  test("Can decode and encode a Case class tree") { test(Gen.resultOf(CaseClass)) }
//  test("Can decode and encode a Case class with defaults set") { testDefaults }
//  test("Can decode and encode a Tuple") { test(implicitly[Arbitrary[(Int, Long)]].arbitrary) }
//  test("Can decode and encode a Option Tuple") { test(Gen.option(implicitly[Arbitrary[(Int, Long)]].arbitrary)) }
//  test("Can load data defaults with standard sync decode") { testDefaultDecode }
//
//  def testNumeric[T: Numeric](
//    implicit encoder: EncT[Validation, T],
//    decoder: DecT[Validation, T],
//    listEncoder: EncT[Validation, List[T]],
//    listDecoder: DecT[Validation, List[T]],
//    tEq: Eq[T],
//    choose: Choose[T]
//  ): Assertion = {
//    testType(Gen.posNum[T])
//    testType(Gen.negNum[T])
//  }
//
//  def testType[T](gen: Gen[T])(
//    implicit encoder: EncT[Validation, T],
//    decoder: DecT[Validation, T],
//    listEncoder: EncT[Validation, List[T]],
//    listDecoder: DecT[Validation, List[T]],
//    tEq: Eq[T]
//  ): Assertion = {
//    test(gen)
//    test(Gen.option(gen))
//    testList(Gen.listOf(gen).suchThat(_.nonEmpty))
//    testNonEmptyList(gen)
//  }
//
//  def testList[T, F[T] <: TraversableOnce[T]](
//    gen: Gen[F[T]]
//  )(implicit encoder: EncT[Validation, F[T]], decoder: DecT[Validation, F[T]]): Assertion =
//    forAll(gen, namespaceGen) { (value, namespace) =>
//      assert((for {
//        encoded <- encodeF[Validation](namespace, value)
//        decoded <- decodeF[Validation, F[T]](namespace, encoded)
//      } yield decoded).map(_.toList === value.filter(_.toString.trim.nonEmpty).toList).right.value)
//    }
//
//  def testNonEmptyList[T](
//    gen: Gen[T]
//  )(implicit encoder: EncT[Validation, NonEmptyList[T]], decoder: DecT[Validation, NonEmptyList[T]]): Assertion =
//    forAll(nonEmptyListGen(gen), namespaceGen) { (value, namespace) =>
//      assert((for {
//        encoded <- encodeF[Validation](namespace, value)
//        decoded <- decodeF[Validation, NonEmptyList[T]](namespace, encoded)
//      } yield decoded).map(_.toList === value.filter(_.toString.trim.nonEmpty)).right.value)
//    }
//
//  def test[T](gen: Gen[T])(
//    implicit encoder: EncT[Validation, T],
//    decoder: DecT[Validation, T],
//    teq: Eq[T],
//    equals: Eq[Validation[T]]
//  ): Assertion = {
//    val F: FlatMap[Validation] = FlatMap[Validation]
//    forAll(gen, namespaceGen) { (value, namespace) =>
//      def eqv(encoded: Validation[OutputData], decoded: InputData => Validation[T]): Boolean = {
//        val x = F.flatMap(encoded)(decoded)
//        equals.eqv(F.flatMap(encoded)(decoded), Validation(Right(value)))
//      }
//
//      assert(eqv(encodeF[Validation](namespace, value), decodeF[Validation, T](namespace, _)))
//      assert(if (supportsEmptyNamespace) eqv(encodeF[Validation](value), decodeF[Validation, T].apply) else true)
//    }
//  }
//
//  def testDefaults: Assertion =
//    forAll(Gen.alphaNumStr, Gen.posNum[Int], Gen.posNum[Long]) { (s, i, l) =>
//      assert(
//        decodeF[Validation, CaseClass](
//          convertData(
//            Map(List("CaseClass", "s") -> s, List("CaseClass", "i") -> i.toString, List("CaseClass", "l") -> l.toString)
//              .map {
//                case (k, v) =>
//                  if (defaultSettings.includeClassNameInPath) k -> v
//                  else k.filterNot(_ == "CaseClass") -> v
//              }
//          )
//        ) === Right(CaseClass(s, i, l, None))
//      )
//    }
//
//  def testDefaultDecode(implicit cvEq: Eq[Validation[CaseClass]]): Assertion = {
//    val test1 = cvEq.eqv(decodeF[Validation, CaseClass].apply, Validation(Right(expectedCaseClass)))
//    val test2 = cvEq.eqv(decodeF[Validation, CaseClass](List.empty), Validation(Right(expectedCaseClass)))
//    assert(test1)
//    assert(test2)
//  }
//
//  def testCaseClassParams: Assertion = assert(parameters[CaseClass] !== "")
//
//  implicit def tuple2Parser[F[_]: Monad, A, B](implicit A: Parser[A], B: Parser[B]): MultiParser[F, (A, B)] =
//    new MultiParser[F, (A, B)] {
//      override def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, (A, B)]] =
//        for {
//          _1 <- lookup(List("_1")).map(A.parseNel)
//          _2 <- lookup(List("_2")).map(B.parseNel)
//        } yield _1.product(_2)
//    }
//
//  implicit def tuple2Show[A, B](implicit A: Show[A], B: Show[B]): MultiShow[(A, B)] = new MultiShow[(A, B)] {
//    override def show(v: (A, B)): Map[List[String], String] =
//      Map(List("_1") -> A.show(v._1), List("_2") -> B.show(v._2))
//  }
//}
