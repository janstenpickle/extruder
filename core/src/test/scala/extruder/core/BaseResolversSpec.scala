package extruder.core

import java.net.URL

import cats.Show
import cats.data.NonEmptyList
import cats.instances.all._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import shapeless.Typeable

import scala.concurrent.duration.{Duration, FiniteDuration}

trait BaseResolversSpec[L] extends Specification with EitherMatchers with ScalaCheck {
  import BaseResolversSpec._

  implicit def showList: Show[L]
  def failingResolvers: Resolvers
  def successfulResolvers[T : Show](value: T): Resolvers
  def makeList[T](list: List[T]): L

  def ext: SpecStructure = s2""""""
  implicit val showUrl: Show[URL] = Show.show(_.toString)
  implicit def showDuration[T <: Duration]: Show[T] = Show.show {
    case x if x == Duration.Inf => "Inf"
    case x if x == Duration.MinusInf => "MinusInf"
    case x => x.toString
  }

  // test is sequential to ensure no concurrent modifications
  override def is: SpecStructure = sequential ^ s2"""
      Succeeds when configuration is present
        String ${succeeding(Gen.alphaNumStr, _.string)}
        Int ${succeeding(Gen.posNum[Int], _.int)}
        Long ${succeeding(Gen.posNum[Long], _.long)}
        Double ${succeeding(Gen.posNum[Double], _.double)}
        Float ${succeeding(Gen.posNum[Float], _.float)}
        Short ${succeeding(Gen.posNum[Short], _.short)}
        Byte ${succeeding(Gen.posNum[Byte], _.byte)}
        Boolean ${succeeding(Gen.oneOf(true, false), _.boolean)}
        URL ${succeeding(urlGen, _.url)}
        Duration ${succeeding[Duration](durationGen, _.duration)}
        Finite Duration ${succeeding[FiniteDuration](finiteDurationGen, _.duration)}

      Fails when configuration is invalid
        Int ${failing(_.int)}
        Long ${failing(_.long)}
        Double ${failing(_.double)}
        Float ${failing(_.float)}
        Short ${failing(_.short)}
        Byte ${failing(_.byte)}
        Boolean ${failing(_.boolean)}
        URL ${failing(_.url)}
        Duration ${failing[Duration](_.duration)}
        Finite Duration ${failing[FiniteDuration](_.duration)}

      When no configuration is available
        Succeeds when there is a default $emptyDefault
        Fails when there is no default $emptyNoDefault

      Can handle scala traversable once collections $collections
      $ext
    """

  def collections: Prop =
    Prop.forAll(Gen.listOf(Gen.posNum[Int])){value =>
      val resolvers = successfulResolvers(makeList(value))
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

  def failing[T : Show](parserSelector: Resolvers => Parser[T]): Prop =
    tester(Gen.alphaStr, runTest[T, String](parserSelector, failure))

  def succeeding[T : Typeable : Show](gen: Gen[T], parserSelector: Resolvers => Parser[T]): Prop =
    tester(gen, runTest[T, T](parserSelector, success))

  def tester[T, V](inputGen: Gen[T], test: T => Result[V]): Prop =
    Prop.forAllNoShrink(inputGen)(test)

  def success[T, V]: Test[T, V] = (result, expected) => result must beRight.which(_ === expected)

  def failure[T, V]: Test[T, V] = (result, _) => result must beLeft

  def runTest[T, V : Show](parserSelector: Resolvers => Parser[T], test: Test[T, V])(expected: V): Result[T] = {
    val resolvers = successfulResolvers(expected)
    test(testResolve[T](resolvers.parseType(parserSelector(resolvers)), None), expected)
  }
}

object BaseResolversSpec {
  type Result[T] = MatchResult[Either[NonEmptyList[ValidationFailure], T]]
  type Test[T, V] = (Either[NonEmptyList[ValidationFailure], T], V) => Result[T]

  val testKey: String = "test"

  val nonEmptyStringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  val urlGen: Gen[URL] = for {
    host <- nonEmptyStringGen
    path <- nonEmptyStringGen
  } yield new URL(s"http://$host/$path")

  val durationGen: Gen[Duration] = Gen.oneOf(
    Gen.choose(Long.MinValue + 1, Long.MaxValue).map(Duration.fromNanos),
    Gen.const(Duration.Inf),
    Gen.const(Duration.MinusInf)
  )

  val finiteDurationGen: Gen[FiniteDuration] = Gen.chooseNum(0L, Long.MaxValue).map(Duration.fromNanos)

  def testResolve[T](resolver: Resolver[T], default: Option[T]): Either[NonEmptyList[ValidationFailure], T] =
    resolver.read(Seq(testKey), default).toEither
}
