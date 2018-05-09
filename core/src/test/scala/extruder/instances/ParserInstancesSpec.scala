package extruder.instances

import cats.Eq
import cats.instances.either._
import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline.FunctorTests
import extruder.core.{Parser, Show}
import org.scalacheck.{Arbitrary, Prop}
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class ParserInstancesSpec extends Specification with Discipline {
  import ParserInstancesSpec._

  override def is: SpecStructure =
    s2"""
       ${checkAll("Parser", FunctorTests[Parser].functor[Int, Int, Int])}
       Can flatMap the parse result $testFlatMapResult
      """

  def testFlatMapResult: Prop = prop { a: Int =>
    Parser[Int].flatMapResult(i => Right(i.toLong)).parse(Show[Int].show(a)) === Right(a.toLong)
  }
}

object ParserInstancesSpec {
  implicit def faArb[A](implicit arb: Arbitrary[A]): Arbitrary[Parser[A]] =
    Arbitrary(arb.arbitrary.map(a => Parser(_ => Right(a))))

  implicit def faEq[A](implicit eq: Eq[Either[String, A]]): Eq[Parser[A]] = Eq.by(_.parse(""))
}
