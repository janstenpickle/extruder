package extruder.cats.effect

import cats.data.EitherT
import cats.effect.laws.discipline.SyncTests
import cats.instances.all._
import cats.{Eq, Eval}
import extruder.data.ValidationErrors
import extruder.laws.ExtruderErrorsTests
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class EvalValidationSuite extends FunSuite with Discipline {
  implicit def evalValidationArb[A](implicit arb: Arbitrary[A]): Arbitrary[EvalValidation[A]] =
    Arbitrary(arb.arbitrary.map(a => EvalValidation(EitherT.rightT[Eval, ValidationErrors](a))))

  implicit def evalValidationEq[A: Eq]: Eq[EvalValidation[A]] = Eq.by(_.a)

  implicit val throwableEq: Eq[Throwable] = Eq.by(_.getMessage)

  checkAll("EvalValidation", ExtruderErrorsTests[EvalValidation].extruderErrors[Int])
  checkAll("EvalValidation", SyncTests[EvalValidation].sync[Int, Int, Int])
}
