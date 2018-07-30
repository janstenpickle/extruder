package extruder.typesafe

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.either._
import cats.kernel.laws.discipline.MonoidTests
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core.TestCommon._
import extruder.core.ValidationCatsInstances._
import extruder.core._
import extruder.effect.ExtruderMonadError
import extruder.typesafe.BaseTypesafeConfigEncoders._
import extruder.typesafe.TypesafeConfigSourceSuite._
import extruder.typesafe.IntermediateTypes._
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion

import scala.collection.JavaConverters._

class TypesafeConfigSourceSuite
    extends StringMapSourceSuite(MonoidTests[Config].monoid)
    with TypesafeConfigDecoders
    with TypesafeConfigEncoders {

  implicit val configValueEq: Eq[ConfigValue] = Eq.fromUniversalEquals
  implicit val configObjectEq: Eq[ConfigObject] = Eq.fromUniversalEquals
  implicit val configListEq: Eq[ConfigList] = Eq.fromUniversalEquals

  override val supportsEmptyNamespace: Boolean = false

//  override val defaultSettings: Sett = new Sett {
//    override def pathToString(path: List[String]): String = TypesafeConfigSource.defaultSettings.pathToString(path)
//  }

  override def convertData(map: Map[List[String], String]): TConfig = {
    val config = map.map { case (k, v) => defaultSettings.pathToString(k) -> v }.asJava
    ConfigFactory.parseMap(config)
  }

  override def loadInput[F[_]](implicit F: ExtruderMonadError[F]): F[TConfig] =
    F.catchNonFatal(convertData(caseClassData))

  test("Can convert ConfigValue") { test[ConfigValue](configValueGen) }
  test("Can convert ConfigObject") { test[ConfigObject](configObjectGen) }
  test("Can convert ConfigList") { test[ConfigList](configListGen) }
  test("Can convert ConfigObjectList") { testList(Gen.listOfN(5, Gen.resultOf(CaseClass))) }

  test("Can encode and decode a list") { testTypesafeList }
  test("Fails to decode a missing list") { testTypsafeListMissing }
  test("Fails to decode an invalid list") { testTypsafeListInvalid }
  test("Fails to convert an invalid type") { testException }

  def testException: Assertion =
    assert(
      decode[String](List(Key), new BrokenConfig(LookupFailureMessage))
        .leftMap(_.head.asInstanceOf[ValidationException].exception.getMessage) === Left(LookupFailureMessage)
    )

  def testTypesafeList: Assertion = forAll { li: List[Int] =>
    assert((for {
      enc <- encode[List[Int]](List("a"), li)
      dec <- decode[List[Int]](List("a"), enc)
    } yield dec) === Right(li))
  }

  def testTypsafeListMissing: Assertion =
    assert(
      decode[List[Int]](List("a")).leftMap(_.head) ===
        Left(Missing("Could not find list at 'a' and no default available"))
    )

  def testTypsafeListInvalid: Assertion = forAll { li: NonEmptyList[String] =>
    assert(
      decode[List[Int]](List("a"), ConfigFactory.parseMap(Map[String, Any]("a" -> li.toList.asJava).asJava))
        .leftMap(_.head)
        .===(Left(ValidationFailure(s"""Could not parse value '${li.toList
          .mkString(", ")}' at 'a': For input string: "${li.head}"""")))
    )
  }
}

object TypesafeConfigSourceSuite {
  val Key = "x"
  val LookupFailureMessage = "boom!"

  case class TestCC(i: Int, s: String)

  val testCCDataGen: Gen[TConfig] = for {
    i <- Gen.posNum[Int]
    s <- Gen.alphaNumStr
  } yield ConfigFactory.parseMap(Map("i" -> i.toString, "s" -> s).asJava)

  implicit val configObjectListGen: Gen[TConfig] =
    configGen[java.util.List[TConfig]](Gen.listOf(testCCDataGen).map(_.asJava))

  val configValueGen: Gen[ConfigValue] =
    configGen(Gen.alphaNumStr).map(_.getValue(Key))

  val configObjectGen: Gen[ConfigObject] =
    configGen(Gen.alphaNumStr).map(_.root())

  val configListGen: Gen[ConfigList] =
    configGen(Gen.listOf(Gen.alphaNumStr).suchThat(_.nonEmpty).map(_.asJava)).map(_.getList(Key))

  def configGen[T](gen: Gen[T]): Gen[TConfig] = gen.map(value => ConfigFactory.parseMap(Map(Key -> value).asJava))

  implicit val configTypesEq: Eq[ConfigTypes] = new Eq[ConfigTypes] {
    override def eqv(x: ConfigTypes, y: ConfigTypes): Boolean = x.eq(y)
  }

  implicit val configTypeArb: Arbitrary[ConfigTypes] =
    Arbitrary(for {
      k <- Gen.alphaNumStr
      v <- Gen.alphaNumStr
    } yield ConfigTypes(k, v))

  implicit val nonEmptyListArb: Arbitrary[NonEmptyList[String]] =
    Arbitrary(for {
      h <- Gen.alphaStr
      t <- Gen.listOf(Gen.alphaStr)
    } yield NonEmptyList(h, t))
}
