package extruder.tests

import java.net.URL

import cats.Eq
import extruder.laws.ParserShowTests
import cats.instances.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

import scala.concurrent.duration.{Duration, FiniteDuration}

class ParserShowSuite extends FunSuite with Discipline {
  import ParserShowSuite._

  checkAll("Char", ParserShowTests[Char].parserShow)
  checkAll("String", ParserShowTests[String].parserShow)
  checkAll("Int", ParserShowTests[Int].parserShow)
  checkAll("Long", ParserShowTests[Long].parserShow)
  checkAll("Double", ParserShowTests[Double].parserShow)
  checkAll("Float", ParserShowTests[Float].parserShow)
  checkAll("Short", ParserShowTests[Short].parserShow)
  checkAll("Byte", ParserShowTests[Byte].parserShow)
  checkAll("Boolean", ParserShowTests[Boolean].parserShow)
  checkAll("URL", ParserShowTests[URL].parserShow)
  checkAll("Duration", ParserShowTests[Duration].parserShow)
  checkAll("FiniteDuration", ParserShowTests[FiniteDuration].parserShow)
  checkAll("List[Int]", ParserShowTests[List[Int]].parserShow)
  checkAll("Set[Int]", ParserShowTests[Set[Int]].parserShow)

}

object ParserShowSuite {

  implicit val finiteDurationArb: Arbitrary[FiniteDuration] = Arbitrary(
    Gen.chooseNum(0L, 5000L).map(Duration.fromNanos)
  )

  implicit val durationArb: Arbitrary[Duration] =
    Arbitrary(
      Gen.oneOf(
        finiteDurationArb.arbitrary,
        Gen.const(Duration.Inf),
        Gen.const(Duration.MinusInf),
        Gen.const(Duration.Zero)
      )
    )

  val nonEmptyStringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  implicit val urlArb: Arbitrary[URL] = Arbitrary(for {
    host <- nonEmptyStringGen
    path <- nonEmptyStringGen
  } yield new URL(s"http://$host/$path"))

  implicit val urlEq: Eq[URL] = Eq.by(_.toString)
}
