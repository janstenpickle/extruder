package extruder.instances

import cats.data.Validated.Valid
import cats.data.{OptionT, ValidatedNel}
import cats.instances.int._
import cats.instances.option._
import cats.laws.discipline.FunctorTests
import cats.{Eq, Id}
import extruder.core.MultiParser
import org.scalacheck.Arbitrary
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class MultiParserInstancesSpec extends Specification with Discipline {
  import MultiParserInstancesSpec._

  override def is: SpecStructure =
    s2"""
        ${checkAll("MultiParser", FunctorTests[MultiParserId].functor[Int, Int, Int])}
      """
}

object MultiParserInstancesSpec {
  type MultiParserId[A] = MultiParser[Id, A]

  implicit def faArb[A](implicit intArb: Arbitrary[A]): Arbitrary[MultiParser[Id, A]] =
    Arbitrary(
      intArb.arbitrary.map(
        i =>
          new MultiParser[Id, A] {
            override def parse(lookup: List[String] => OptionT[Id, String]): OptionT[Id, ValidatedNel[String, A]] =
              OptionT.pure(Valid(i))
        }
      )
    )

  implicit def faEq[A](implicit intEq: Eq[Option[A]]): Eq[MultiParser[Id, A]] =
    Eq.by(_.parse(_ => OptionT.none).value.flatMap(_.toOption))
}
