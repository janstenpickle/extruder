package extruder.core

import cats.data.NonEmptyList
import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure
import extruder.syntax.validation.ConfigValidation
import cats.syntax.validated._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}

class ResolversSpecextends extends Specification with EitherMatchers with ScalaCheck {
  import ResolversSpec._
  import Resolvers._

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

      Can handle scala traversable once collections $collections
    """

  def collections: Prop =
    Prop.forAll(Gen.listOf(Gen.posNum[Int])){value =>
      val resolvers = successfulResolvers(value.mkString(","))
      import resolvers.int
      (testResolve[List[Int]](resolvers.parseTraversable, None) must beRight(value)) and
      (testResolve[Set[Int]](resolvers.parseTraversable, None) must beRight.which(_ === value.toSet)) and
      (testResolve[Seq[Int]](resolvers.parseTraversable, None) must beRight(value.toSeq)) and
      (testResolve[IndexedSeq[Int]](resolvers.parseTraversable, None) must beRight(value.toIndexedSeq)) and
      (testResolve[Iterable[Int]](resolvers.parseTraversable, None) must beRight(value.toIterable)) and
      (testResolve[Stream[Int]](resolvers.parseTraversable, None) must beRight(value.toStream)) and
      (testResolve[Vector[Int]](resolvers.parseTraversable, None) must beRight(value.toVector))
    }

  def emptyDefault: Prop = Prop.forAll(Gen.alphaNumStr)(default =>
    testResolve(failingResolvers.parseType(failingResolvers.string), Some(default)) must beRight.like {
      case a => a === default
    }
  )

  def emptyNoDefault: Result[String] =
    testResolve(failingResolvers.parseType(failingResolvers.string), None) must beLeft.which(a =>
      a.toList.size === 1 and
      a.head === new ValidationFailure("Could not find configuration at 'test' and no default available", None)
    )

  def failing[T](parserSelector: Resolvers => Parser[T]): Prop =
    tester(Gen.alphaStr, runTest(parserSelector, failure))

  def succeeding[T](gen: Gen[T], parserSelector: Resolvers => Parser[T]): Prop =
    tester(gen.map(_.toString), runTest(parserSelector, success))

  def tester[T, V](inputGen: Gen[String], test: String => Result[V]): Prop =
    Prop.forAll(inputGen)(test)

  def success[T]: Test[T] = (result, expected) => result must beRight.which(_.toString === expected)

  def failure[T]: Test[T] = (result, _) => result must beLeft

  def runTest[T](parserSelector: Resolvers => Parser[T], test: Test[T])(expected: String): Result[T] = {
    val resolvers = successfulResolvers(expected)
    test(testResolve[T](resolvers.parseType(parserSelector(resolvers)), None), expected)
  }
}

object ResolversSpec {
  type Result[T] = MatchResult[Either[NonEmptyList[ValidationFailure], T]]
  type Test[T] = (Either[NonEmptyList[ValidationFailure], T], String) => Result[T]

  val failingResolvers = new Resolvers {
    override def pathToString(path: Seq[String]): String = path.mkString("").toLowerCase
    override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = None.validNel
  }

  def successfulResolvers(value: String) = new Resolvers {
    override def pathToString(path: Seq[String]): String = ""
    override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = Some(value).validNel
  }

  def testResolve[T](resolver: Resolver[T], default: Option[T]): Either[NonEmptyList[ValidationFailure], T] =
    resolver.read(Seq("test"), default).toEither
}
