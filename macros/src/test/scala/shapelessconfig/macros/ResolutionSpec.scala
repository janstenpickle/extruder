package shapelessconfig.macros

import cats.data.NonEmptyList
import cats.syntax.validated._
import org.specs2.Specification
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import shapelessconfig.core.Resolvers
import shapelessconfig.macros.resolution._
import shapelessconfig.syntax.validation.ConfigValidation

class ResolutionSpec extends Specification with EitherMatchers {
  import ResolutionSpec._

  override def is: SpecStructure = s2"""
    Can resolve a case class from config
      With full path $fullPath
      As singletons $singletons
  """

  def fullPath: Result = test(resolve[Test1](resolver), Test1(Test2(Test3("test1-test2-test3")), Test3("test1-test3")))

  def singletons: Result = test(resolve[Test1].singletons(resolver), Test1(Test2(Test3("test3")), Test3("test3")))

  def test(result: ConfigValidation[Test1], expected: Test1): Result =
    result.toEither must beRight.which(_ === expected)
}

object ResolutionSpec {
  type Result = MatchResult[Either[NonEmptyList[String], Test1]]

  val config = Map(
    "test1.a.test2.a.test3.a" -> "test1-test2-test3",
    "test1.b.test3.a" -> "test1-test3",
    "test3.a" -> "test3"
  )

  val resolver = new Resolvers {
    override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
    override def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]] =
      config.get(pathToString(path)).valid
  }

  case class Test1(a: Test2, b: Test3)
  case class Test2(a: Test3)
  case class Test3(a: String)
}
