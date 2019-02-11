package extruder.tests

import cats.data.EitherT
import cats.instances.all._
import cats.syntax.either._
import cats.{Eq, Id}
import extruder.data.{ValidationErrors, ValidationT}
import extruder.laws.ExtruderErrorsTests
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ValidationTSuite extends FunSuite with Discipline {
  type Vid[A] = ValidationT[Id, A]

  implicit def validationTArb[A](implicit arb: Arbitrary[A]): Arbitrary[ValidationT[Id, A]] =
    Arbitrary(
      arb.arbitrary.map(a => ValidationT[Id, A](EitherT[Id, ValidationErrors, A](Either.right[ValidationErrors, A](a))))
    )

  implicit def validationTEq[A: Eq]: Eq[ValidationT[Id, A]] = Eq.by(_.a)

  checkAll("ValidationT", ExtruderErrorsTests[Vid].extruderErrors[Int])
}
