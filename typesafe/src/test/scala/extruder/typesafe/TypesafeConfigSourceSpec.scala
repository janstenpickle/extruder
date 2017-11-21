package extruder.typesafe

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.either._
import cats.kernel.laws.discipline.MonoidTests
import com.typesafe.config._
import extruder.core.ValidationCatsInstances._
import extruder.core._
import extruder.effect.ExtruderMonadError
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure
import shapeless.Coproduct

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

  override def loadInput[F[_]](implicit F: ExtruderMonadError[F]): F[Config] =
    F.catchNonFatal(convertData(caseClassData))

  override def ext: SpecStructure =
    s2"""
        Can convert typesafe specific types
          ConfigValue ${test[ConfigValue](configValueGen)}
          ConfigObject ${test[ConfigObject](configObjectGen)}
          ConfigList ${test[ConfigList](configListGen)}

        Can encode and decode a list $testTypesafeList
        Fails to decode a missing list $testTypsafeListMissing
        Fails to decode an invalid list $testTypsafeListInvalid

        Fails to convert an invalid type $testException
      """

  def testException: MatchResult[Any] =
    decode[String](List(Key), new BrokenConfig(LookupFailureMessage)) must beLeft.which(
      _.head.asInstanceOf[ValidationException].exception.getMessage === LookupFailureMessage
    )

  def testTypesafeList: Prop = prop { (li: List[Int]) =>
    (for {
      enc <- encode[List[Int]](List("a"), li)
      dec <- decode[List[Int]](List("a"), enc)
    } yield dec) must beRight.which(_ === li)
  }

  def testTypsafeListMissing: MatchResult[Validation[List[Int]]] =
    decode[List[Int]](List("a")) must beLeft.which(
      _.head === Missing("Could not find list at 'a' and no default available")
    )

  def testTypsafeListInvalid: Prop = prop { (li: NonEmptyList[String]) =>
    decode[List[Int]](List("a"), ConfigFactory.parseMap(Map[String, Any]("a" -> li.toList.asJava).asJava)) must beLeft
      .which(_.head === ValidationFailure(s"""Could not parse value '${li.toList
        .mkString(", ")}' at 'a': For input string: "${li.head}""""))
  }

  override def monoidTests: MonoidTests[ConfigMap]#RuleSet = MonoidTests[ConfigMap](monoid).monoid

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

  implicit val nonEmptyListArb: Arbitrary[NonEmptyList[String]] =
    Arbitrary(for {
      h <- Gen.alphaStr
      t <- Gen.listOf(Gen.alphaStr)
    } yield NonEmptyList(h, t))
}
