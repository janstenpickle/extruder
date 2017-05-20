package extruder.typesafe

import cats.Eq
import cats.kernel.laws.GroupLaws
import cats.instances.all._
import com.typesafe.config._
import extruder.core.{ConfigSpec, ValidationException}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure
import shapeless._

import scala.collection.JavaConverters._

class TypesafeConfigSpec extends ConfigSpec[Config, Config, ConfigMap, TypesafeConfigDecoder, TypesafeConfigEncoder]
                         with TypesafeConfigDecoders
                         with TypesafeConfigEncoders {
  import TypesafeConfigSpec._

  override def convertConfig(map: Map[Seq[String], String]): Config = {
    val config = map.map { case (k, v) => utils.pathToString(k) -> v }.asJava
    ConfigFactory.parseMap(config)
  }

  override def ext: SpecStructure =
    s2"""
        Can convert typesafe specific types
          ConfigValue ${test[ConfigValue](configValueGen)}
          ConfigObject ${test[ConfigObject](configObjectGen)}
          ConfigList ${test[ConfigList](configListGen)}

        Fails to convert an invalid type $testException
      """

  def testException: MatchResult[Any] =
    decode[String](Seq(Key), new BrokenConfig(LookupFailureMessage)).toEither must beLeft.which(
      _.head.asInstanceOf[ValidationException].exception.getMessage === LookupFailureMessage
    )

  override def monoidGroupLaws: GroupLaws[ConfigMap] = GroupLaws[ConfigMap]
}

object TypesafeConfigSpec {
  val Key = "x"
  val LookupFailureMessage = "boom!"

  val configValueGen: Gen[ConfigValue] =
    configGen(Gen.alphaNumStr).map(_.getValue(Key))

  val configObjectGen: Gen[ConfigObject] =
    configGen(Gen.alphaNumStr).map(_.root())

  val configListGen: Gen[ConfigList] =
    configGen(Gen.listOf(Gen.alphaNumStr).suchThat(_.nonEmpty).map(_.asJava)).map(_.getList(Key))

  def configGen[T](gen: Gen[T]): Gen[Config] = gen.map(value => ConfigFactory.parseMap(Map(Key -> value).asJava))

  implicit val configTypesEq: Eq[ConfigTypes] = new Eq[ConfigTypes] {
    override def eqv(x: ConfigTypes, y: ConfigTypes): Boolean = x.eq(y)
  }

  implicit val configTypeArb: Arbitrary[ConfigTypes] =
    Arbitrary(Gen.alphaNumStr.map(Coproduct[ConfigTypes].apply[String]))
}
