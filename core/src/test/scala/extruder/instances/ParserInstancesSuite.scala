package extruder.instances

import cats.Eq
import cats.instances.either._
import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline.AlternativeTests
import extruder.core.{Parser, Show}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline

class ParserInstancesSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Discipline {
  import ParserInstancesSuite._

  checkAll("Parser", AlternativeTests[Parser].alternative[Int, Int, Int])

  test("Can flatMap the parse result") {
    forAll { a: Int =>
      assert(Parser[Int].flatMapResult(i => Right(i.toLong)).parse(Show[Int].show(a)) === Right(a.toLong))
    }
  }
}

object ParserInstancesSuite {
  implicit def faArb[A](implicit arb: Arbitrary[A]): Arbitrary[Parser[A]] =
    Arbitrary(arb.arbitrary.map(a => Parser(_ => Right(a))))

  implicit def fabcEq: Eq[Parser[(Int, Int, Int)]] = new Eq[Parser[(Int, Int, Int)]] {
    override def eqv(x: Parser[(Int, Int, Int)], y: Parser[(Int, Int, Int)]): Boolean =
      x.parse("") == y.parse("")
  }

  implicit def faEq[A](implicit eq: Eq[Either[String, A]]): Eq[Parser[A]] = Eq.by(_.parse(""))
}
