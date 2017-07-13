package extruder.instances

import cats.Eq
import cats.instances.all._
import cats.laws.discipline.CartesianTests.Isomorphisms
import cats.laws.discipline.FlatMapTests
import cats.syntax.validated._
import extruder.core.ValidationCatsInstances._
import extruder.core.{Validation, ValidationError}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class ValidationFlatMapSpec extends Specification with Discipline with ValidationInstances {
  implicit val validationErrorEq: Eq[ValidationError] = Eq[String].on(_.toString)
  implicit def iso: Isomorphisms[Validation] = Isomorphisms.invariant[Validation](validationFlatMap)
  implicit def FaArb[A](implicit tarb: Arbitrary[A]): Arbitrary[Validation[A]] =
    Arbitrary(Gen.oneOf(validationErrorsArb.arbitrary.map(_.invalid), tarb.arbitrary.map(_.valid)))
  val test: FlatMapTests[Validation] = FlatMapTests[Validation]

  override def is: SpecStructure = checkAll("Validation FlatMap instance", test.flatMap[Int, Int, Int])
}
