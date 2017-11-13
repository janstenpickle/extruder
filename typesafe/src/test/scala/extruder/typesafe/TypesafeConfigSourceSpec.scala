package extruder.typesafe

import cats.Eq
import cats.instances.all._
import cats.kernel.laws.GroupLaws
import com.typesafe.config._
import extruder.core.ValidationCatsInstances._
import extruder.core.{ExtruderEffect, SourceSpec, ValidationException}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure
import shapeless._

import scala.collection.JavaConverters._

class TypesafeConfigSourceSpec extends SourceSpec with TypesafeConfigDecoders with TypesafeConfigEncoders {
  import TypesafeConfigSourceSpec._

  implicit val configValueEq: Eq[ConfigValue] = Eq.fromUniversalEquals
  implicit val configObjectEq: Eq[ConfigObject] = Eq.fromUniversalEquals
  implicit val configListEq: Eq[ConfigList] = Eq.fromUniversalEquals

  override val supportsEmptyNamespace: Boolean = false

  override def convertData(map: Map[List[String], String])(implicit hints: Hint): Config = {
    val config = map.map { case (k, v) => hints.pathToString(k) -> v }.asJava
    ConfigFactory.parseMap(config)
  }

  override def loadInput[F[_]](implicit F: ExtruderEffect[F]): F[InputData] = F.delay(convertData(caseClassData))

  override def ext: SpecStructure =
    s2"""
        Can convert typesafe specific types
          ConfigValue ${test[ConfigValue](configValueGen)}
          ConfigObject ${test[ConfigObject](configObjectGen)}
          ConfigList ${test[ConfigList](configListGen)}

        Fails to convert an invalid type $testException
      """

  def testException: MatchResult[Any] =
    decode[String](List(Key), new BrokenConfig(LookupFailureMessage)).toEither must beLeft.which(
      _.head.asInstanceOf[ValidationException].exception.getMessage === LookupFailureMessage
    )

  override def monoidGroupLaws: GroupLaws[ConfigMap] = GroupLaws[ConfigMap]

  override implicit def hints: TypesafeConfigHints = TypesafeConfigHints.default
}

object TypesafeConfigSourceSpec {
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
