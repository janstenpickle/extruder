package extruder.core

import cats.data.NonEmptyList
import org.specs2.Specification
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import cats.syntax.validated._

class ExtruderSpec extends Specification with EitherMatchers {
  import ExtruderSpec._
  import resolvers._

  override def is: SpecStructure = s2"""
    Can successfully construct a case class from
      Config alone ${testSuccess(Extruder[NoDefaults]().resolve, NoDefaults("nic cage", 1, isAwesome = true))}
      Defaults alone ${testSuccess(Extruder[WithDefaults]().resolve, WithDefaults())}
      Config and defaults ${testSuccess(Extruder[SomeDefaults]().resolve, SomeDefaults("nic cage"))}
      Overridden defaults ${testSuccess(Extruder[OverriddenDefaults]().resolve, OverriddenDefaults("nic cage", 1, isAwesome = true))}

    Fails to construct a case class when
      No config exists ${testFailure(Extruder[NoDefaults](failPrefixVal).resolve, 3)}
      Configuration cannot be parsed correctly ${testFailure(Extruder[WithDefaults](failPrefixVal).resolve, 1)}
  """

  def testSuccess[T](input: ConfigValidation[T], expected: T): MatchResult[Either[NonEmptyList[ValidationFailure], T]] =
    input.toEither must beRight.which(_ === expected)

  def testFailure[T](input: ConfigValidation[T], errors: Int): MatchResult[Either[NonEmptyList[ValidationFailure], T]] =
    input.toEither must beLeft.which(_.toList.size === errors)
}

object ExtruderSpec {
  val failPrefix = "fail"
  val failPrefixVal = Some(Seq(failPrefix))

  val validConfig = Map(
    "nodefaults.name" -> "nic cage",
    "nodefaults.oscars" -> "1",
    "nodefaults.isawesome" -> "true",
    "somedefaults.name" -> "nic cage",
    "overriddendefaults.name" -> "nic cage",
    "overriddendefaults.oscars" -> "1",
    "overriddendefaults.isawesome" -> "true",
    s"$failPrefix.withdefaults.oscars" -> "not an int"
  )

  val resolvers =  new Resolvers {
    override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
    override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] =
      validConfig.get(pathToString(path)).validNel
  }

  case class NoDefaults(name: String, oscars: Int, isAwesome: Boolean)
  case class WithDefaults(name: String = "john travolta", oscars: Int = 0, isAwesome: Boolean = false)
  case class SomeDefaults(name: String, oscars: Int = 0, isAwesome: Boolean = true)
  case class OverriddenDefaults(name: String = "john travolta", oscars: Int = 0, isAwesome: Boolean = false)
}
