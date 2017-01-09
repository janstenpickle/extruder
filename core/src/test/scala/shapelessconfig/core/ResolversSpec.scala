package shapelessconfig.core

import cats.data.NonEmptyList
import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure
import shapelessconfig.syntax.validation.ConfigValidation
import cats.syntax.validated._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}

class ResolversSpec extends Specification with EitherMatchers with ScalaCheck {
  import ResolversSpec._

  override def is: SpecStructure = s2"""
      Succeeds when configuration is present
        String ${succeeding(Gen.alphaNumStr, _.string)}
        Int ${succeeding(Gen.posNum[Int], _.int)}
        Long ${succeeding(Gen.posNum[Long], _.long)}
        Double ${succeeding(Gen.posNum[Double], _.double)}
        Float ${succeeding(Gen.posNum[Float], _.float)}
        Short ${succeeding(Gen.posNum[Short], _.short)}
        Byte ${succeeding(Gen.posNum[Byte], _.byte)}
        Boolean ${succeeding(Gen.oneOf(true, false), _.boolean)}

      Fails when configuration is invalid
        Int ${failing(_.int)}
        Long ${failing(_.long)}
        Double ${failing(_.double)}
        Float ${failing(_.float)}
        Short ${failing(_.short)}
        Byte ${failing(_.byte)}
        Boolean ${failing(_.boolean)}

      When no configuration is available
        Succeeds when there is a default $emptyDefault
        Fails when there is no default $emptyNoDefault
    """

  def emptyDefault: Prop = Prop.forAll(Gen.alphaNumStr)(default =>
    testResolve(failingResolvers.string, Some(default)) must beRight.like {
      case a => a === default
    }
  )

  def emptyNoDefault: Result[String] =
    testResolve(failingResolvers.string, None) must beLeft.which(a =>
      a.toList.size === 1 and
      a.head === "Could not find configuration at 'test' and no default available"
    )


  def failing[T](resolverSelector: Resolvers => Resolver[T]): Prop =
    tester(Gen.alphaStr, runTest(resolverSelector, failure))

  def succeeding[T](gen: Gen[T], resolverSelector: Resolvers => Resolver[T]): Prop =
    tester(gen.map(_.toString), runTest(resolverSelector, success))

  def tester[T, V](inputGen: Gen[String], test: String => Result[V]): Prop =
    Prop.forAll(inputGen)(test)

  def success[T]: Test[T] = (result, expected) => result must beRight.which(_.toString === expected)

  def failure[T]: Test[T] = (result, _) => result must beLeft

  def runTest[T](resolverSelector: Resolvers => Resolver[T], test: Test[T])(expected: String): Result[T] =
    test(testResolve[T](resolverSelector(successfulResolvers(expected)), None), expected)
}

object ResolversSpec {
  type Result[T] = MatchResult[Either[NonEmptyList[String], T]]
  type Test[T] = (Either[NonEmptyList[String], T], String) => Result[T]

  val failingResolvers = new Resolvers {
    override def pathToString(path: Seq[String]): String = path.mkString("").toLowerCase
    override def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]] = None.validNel
  }

  def successfulResolvers(value: String) = new Resolvers {
    override def pathToString(path: Seq[String]): String = ""
    override def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]] = Some(value).validNel
  }

  def testResolve[T](resolver: Resolver[T], default: Option[T]): Either[NonEmptyList[String], T] =
    resolver.read(Seq("test"), default).toEither
}
