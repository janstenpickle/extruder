package extruder.typesafe

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.all._
import cats.Monoid
import cats.kernel.laws.discipline.MonoidTests
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core.Settings
import extruder.data.Validation
import extruder.laws.{CaseClass, EncoderDecoderGenericTests, EncoderDecoderTests}
import extruder.typesafe.IntermediateTypes.{Config, ConfigTypes}
import org.scalatest.{EitherValues, FunSuite}
import org.typelevel.discipline.scalatest.Discipline
import extruder.data.ValidationError.{Missing, ValidationException, ValidationFailure}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.compatible.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.ScalacheckShapeless._

import scala.collection.JavaConverters._

class TypesafeConfigSuite extends FunSuite with EitherValues with GeneratorDrivenPropertyChecks with Discipline {
  import TypesafeConfigSuite._

  val encodeDecodeTests: EncoderDecoderTests[Validation, Settings, Config, TConfig, TConfig] =
    EncoderDecoderTests[Validation, Settings, Config, TConfig, TConfig](defaultSettings)

  checkAll("Config Repr Monoid", MonoidTests[Config].monoid)

  checkAll(
    "TypesafeConfig",
    EncoderDecoderGenericTests[Validation, Settings, Config, TConfig, TConfig](defaultSettings)
      .genericEncodeDecode[Int, Int, String]
  )

  checkAll("TypesafeConfig ConfigObject", encodeDecodeTests.encodeDecode[ConfigObject])
  checkAll("TypesafeConfig ConfigValue", encodeDecodeTests.encodeDecode[ConfigValue])
  checkAll("TypesafeConfig ConfigList", encodeDecodeTests.encodeDecode[ConfigList])
  checkAll("TypesafeConfig Object List", encodeDecodeTests.encodeDecode[List[CaseClass]])

  test("Fails to decode a missing list") { testTypsafeListMissing }
  test("Fails to decode an invalid list") { testTypsafeListInvalid }
  test("Fails to convert an invalid type") { testException }

  def testException: Assertion =
    assert(
      decode[String](List(Key), new BrokenConfig(LookupFailureMessage)).left.value.head
        .asInstanceOf[ValidationException]
        .exception
        .getMessage === LookupFailureMessage
    )

  def testTypsafeListMissing: Assertion =
    assert(
      decode[List[Int]](List("a")).left.value.head ===
        Missing("Could not find value at 'a' and no default available")
    )

  def testTypsafeListInvalid: Assertion = forAll { li: NonEmptyList[String] =>
    assert(
      decode[List[Boolean]](List("a"), ConfigFactory.parseMap(Map[String, Any]("a" -> li.toList.asJava).asJava)).left.value.head
        .===(ValidationFailure(s"""Could not parse value '${li.toList
          .mkString(", ")}' at 'a': For input string: "${li.head}""""))
    )
  }
}

object TypesafeConfigSuite {

  val Key = "x"

  val LookupFailureMessage = "boom!"

  implicit val tConfigMonoid: Monoid[TConfig] = new Monoid[TConfig] {
    override def empty: TConfig = ConfigFactory.empty()
    override def combine(x: TConfig, y: TConfig): TConfig = x.withFallback(y)
  }

  def configGen[T](gen: Gen[T]): Gen[TConfig] = gen.map(value => ConfigFactory.parseMap(Map(Key -> value).asJava))

  implicit val configValueArb: Arbitrary[ConfigValue] =
    Arbitrary(configGen(Gen.alphaNumStr).map(_.getValue(Key)))

  implicit val configValueEq: Eq[ConfigValue] = Eq.by(_.render())

  implicit val configListArb: Arbitrary[ConfigList] =
    Arbitrary(configGen(Gen.listOf(Gen.alphaNumStr).suchThat(_.nonEmpty).map(_.asJava)).map(_.getList(Key)))

  implicit val configListEq: Eq[ConfigList] = Eq.by(_.render())

  implicit val configObjectArb: Arbitrary[ConfigObject] =
    Arbitrary(configGen(Gen.alphaNumStr).map(_.root()))

  implicit val configObjectEq: Eq[ConfigObject] = Eq.by(_.render())

  implicit val configTypesEq: Eq[ConfigTypes] = new Eq[ConfigTypes] {
    override def eqv(x: ConfigTypes, y: ConfigTypes): Boolean = x.eq(y)
  }

  implicit val configTypeArb: Arbitrary[ConfigTypes] =
    Arbitrary(for {
      k <- Gen.alphaNumStr
      v <- Gen.alphaNumStr
    } yield ConfigTypes(k, v))

  implicit val nelArb: Arbitrary[NonEmptyList[String]] =
    Arbitrary(for {
      h <- Gen.alphaNumStr
      t <- Gen.listOf(Gen.alphaNumStr)
    } yield NonEmptyList.of(h, t: _*))
}
