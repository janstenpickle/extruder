package extruder.typesafe

import java.net.URL
import java.util

import cats.Show
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.show._
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue}
import extruder.core.{BaseResolversSpec, Resolvers, ValidationFailure}
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration

class TypesafeConfigResolversSpec extends BaseResolversSpec[java.util.List[String]] {
  import BaseResolversSpec._
  import TypesafeConfigResolversSpec._

  override implicit def showList: Show[util.List[String]] = Show.show(_.toString)

  implicit val showMap: Show[java.util.Map[String, String]] = Show.show(_.toString)

  override val failingResolvers: Resolvers =
    TypesafeConfigResolvers(ConfigFactory.parseMap(Map.empty[String, Any].asJava))

  def maybeShow[T : Show](value: T): Any = value match {
    case _: URL => value.show
    case _: Duration => value.show
    case _ => value
  }

  override def successfulResolvers[T : Show](value: T): TypesafeConfigResolvers =
    TypesafeConfigResolvers(ConfigFactory.parseMap(Map(testKey -> maybeShow(value)).asJava))

  override def makeList[T](list: List[T]): java.util.List[String] = list.map(_.toString).asJava

  override def ext: SpecStructure =
    s2"""
      Typesafe specific types
        Can resolve ConfigValue $configValue
        Can resolve ConfigList $configList
        Can resolve ConfigObject $configObject

      Fails when the underlying config implementation throws an exception $failingConfig
      """

  def failingConfig: MatchResult[Either[NonEmptyList[ValidationFailure],Option[String]]] =
    TypesafeConfigResolvers(new BrokenConfig()).lookup(_.getString(""), Seq.empty).toEither must beLeft.which(err =>
      err.toList.size === 1
    )

  def configValue: Prop = Prop.forAll(Gen.alphaNumStr) (value =>
    testResolve[ConfigValue](successfulResolvers(value).configValueResolver, None) must beRight.
      which(_.unwrapped() === value)
  )

  def configList: Prop = Prop.forAll(Gen.listOf(Gen.alphaNumStr))(value =>
    testResolve[ConfigList](successfulResolvers(value.asJava).configListResolver, None) must beRight.
      which(_.unwrapped().asScala.toList === value)
  )


  def configObject: Prop = Prop.forAll(Gen.mapOf(nonEmptyString.map(s => (s, s))))(value =>
    testResolve[ConfigObject](successfulResolvers(value.asJava).configObjectResolver, None) must beRight.
      which(_.unwrapped().asScala.toMap === value)
  )
}

object TypesafeConfigResolversSpec {
  val nonEmptyString: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)
}
