package extruder.instances

import cats.Eq
import cats.instances.either._
import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline.FunctorTests
import extruder.core.{Parser, Show}
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline

class ParserInstancesSuite extends FunSuite with Discipline with GeneratorDrivenPropertyChecks {
  import ParserInstancesSuite._

  checkAll("Parser", FunctorTests[Parser].functor[Int, Int, Int])

  test("Can flatMap the parse result") {
    forAll { a: Int =>
      assert(Parser[Int].flatMapResult(i => Right(i.toLong)).parse(Show[Int].show(a)) === Right(a.toLong))
    }
  }
}

object ParserInstancesSuite {
  implicit def faArb[A](implicit arb: Arbitrary[A]): Arbitrary[Parser[A]] =
    Arbitrary(arb.arbitrary.map(a => Parser(_ => Right(a))))

  implicit def faEq[A](implicit eq: Eq[Either[String, A]]): Eq[Parser[A]] = Eq.by(_.parse(""))
}
