package extruder

import cats.data.NonEmptyList
import extruder.core.{ConfigValidation, MapResolvers, ValidationFailure}
import extruder.resolution._
import org.specs2.Specification
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure

object ResolutionSpec {
  type Result[T] = MatchResult[Either[NonEmptyList[ValidationFailure], T]]

  val config = Map(
    "test1.a.test2.a.test3.a" -> "test1-test2-test3",
    "test1.b.test3.a" -> "test1-test3",
    "test3.a" -> "test3",
    "type" -> "SealedObj",
    "listtest.a" -> "a,b,c"
  )

  val resolver = MapResolvers(config)

  case class Test1(a: Test2, b: Test3)
  case class Test2(a: Test3)
  case class Test3(a: String)

  sealed trait Sealed
  case object SealedObj extends Sealed
  case class SealedCC(a: Test3) extends Sealed

  case class OptTest(a: Option[String])
  case class ListTest(a: List[String])
}

class ResolutionSpec extends Specification with EitherMatchers {
  import ResolutionSpec._

  override def is: SpecStructure = s2"""
    Can resolve a case class from config $caseClasses
    Can resolve a sealed family $sealedClasses
    Can resolve optional parameters in case classes $optionalParams
    Can resolve traversable once parameters in case classes $listParams
  """

  def caseClasses: Result[Test1] =
    test(resolve[Test1, MapResolvers](resolver), Test1(Test2(Test3("test1-test2-test3")), Test3("test1-test3")))

  def sealedClasses: Result[Sealed] = test(resolve[Sealed, MapResolvers](resolver), SealedObj)

  def optionalParams: Result[OptTest] = test(resolve[OptTest, MapResolvers](resolver), OptTest(None))

  def listParams: Result[ListTest] = test(resolve[ListTest, MapResolvers](resolver), ListTest(List("a", "b", "c")))

  def test[T](result: ConfigValidation[T], expected: T): Result[T] =
    result.toEither must beRight(expected)
}
