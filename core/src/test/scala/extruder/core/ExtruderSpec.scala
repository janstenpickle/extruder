package extruder.core

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import org.specs2.Specification
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure

object ExtruderSpec {
  val failPrefix = "fail"

  val validConfig = Map(
    "nodefaults.name" -> "nic cage",
    "nodefaults.oscars" -> "1",
    "nodefaults.isawesome" -> "true",
    "somedefaults.name" -> "nic cage",
    "overriddendefaults.name" -> "nic cage",
    "overriddendefaults.oscars" -> "1",
    "overriddendefaults.isawesome" -> "true",
    "invalidconfig.withdefaults.oscars" -> "not an int",
    "type" -> "Obj"
  )

  val resolvers = MapResolvers(validConfig)

  case class NoDefaults(name: String, oscars: Int, isAwesome: Boolean)
  case class WithDefaults(name: String = "john travolta", oscars: Int = 0, isAwesome: Boolean = false)
  case class SomeDefaults(name: String, oscars: Int = 0, isAwesome: Boolean = true)
  case class OverriddenDefaults(name: String = "john travolta", oscars: Int = 0, isAwesome: Boolean = false)

  case class NoDefaultsFail(name: String, oscars: Int, isAwesome: Boolean)
  case class InvalidConfig(name: String = "john travolta", oscars: Int, isAwesome: Boolean = false)

  sealed trait Tst
  case object Obj extends Tst

  implicit val objResolver: Resolver[Obj.type] = Resolver(Valid(Obj))

  case class WithSealed(t: Tst)
  case class WithSealedDefault(t: Tst = Obj)
}

class ExtruderSpec extends Specification with EitherMatchers {
  import ExtruderSpec._
  import resolvers._

  implicit val tstResolver: Resolver[Tst] = Extruder[Tst](resolvers).unionResolver

  override def is: SpecStructure = s2"""
    Can successfully construct a case class from
      Config alone ${testSuccess(Extruder[NoDefaults](resolvers).productResolver, NoDefaults("nic cage", 1, isAwesome = true))}
      Defaults alone ${testSuccess(Extruder[WithDefaults](resolvers).productResolver, WithDefaults())}
      Config and defaults ${testSuccess(Extruder[SomeDefaults](resolvers).productResolver, SomeDefaults("nic cage"))}
      Overridden defaults ${testSuccess(Extruder[OverriddenDefaults](resolvers).productResolver, OverriddenDefaults("nic cage", 1, isAwesome = true))}
      Sealed member ${testSuccess(Extruder[Tst](resolvers).unionResolver, Obj)}
      Defaulted sealed member ${testSuccess(Extruder[WithSealedDefault](resolvers).productResolver, WithSealedDefault(Obj))}

    Fails to construct a case class when
      No config exists ${testFailure(Extruder[NoDefaultsFail](resolvers).productResolver, 3)}
      Configuration cannot be parsed correctly ${testFailure(Extruder[InvalidConfig](resolvers).productResolver, 1)}
      Sealed type is not specified ${testFailure(Extruder[WithSealed](MapResolvers(Map.empty)).productResolver, 1)}
      Sealed type does not exist ${testFailure(Extruder[WithSealed](MapResolvers(Map("withsealed.t.type" -> "nothing"))).productResolver, 1)}
  """

  def testSuccess[T](input: Resolver[T], expected: T): MatchResult[Either[NonEmptyList[ValidationFailure], T]] =
    input.read(Seq.empty, None).toEither must beRight.which(_ === expected)

  def testFailure[T](input: Resolver[T], errors: Int): MatchResult[Either[NonEmptyList[ValidationFailure], T]] =
    input.read(Seq.empty, None).toEither must beLeft.which(_.toList.size === errors)
}
