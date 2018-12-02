package extruder.data

import cats.laws.discipline.MonadErrorTests
import org.scalatest.FunSuite
import cats.implicits._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.scalatest.Discipline

class ValidationSuite extends FunSuite with Discipline {
  import ValidationCatsInstances._

  implicit def arb[A](implicit arb: Arbitrary[A]) = Arbitrary(arb.arbitrary.map(a => Validation(Right(a))))

  checkAll("validation", MonadErrorTests[Validation, Throwable].monadError[Int, Int, Int])
}
