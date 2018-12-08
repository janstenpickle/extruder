package extruder.tests

import cats.instances.all._
import extruder.data.Validation
import extruder.laws.ExtruderErrorsTests
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ValidationSuite extends FunSuite with Discipline {
  implicit def validationArb[A](implicit arb: Arbitrary[A]): Arbitrary[Validation[A]] =
    Arbitrary(arb.arbitrary.map(a => Validation(Right(a))))

  checkAll("Validation", ExtruderErrorsTests[Validation].extruderErrors[Int])
}
