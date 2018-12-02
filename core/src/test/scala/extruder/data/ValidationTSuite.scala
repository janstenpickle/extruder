package extruder.data

import cats.Eq
import cats.data.{Chain, EitherT}
import cats.implicits._
import cats.laws.discipline.MonadErrorTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ValidationTSuite extends FunSuite with Discipline {
  import ValidationCatsInstances._

  implicit def arb[A](implicit arb: Arbitrary[A]) =
    Arbitrary(arb.arbitrary.map(a => ValidationT(EitherT.pure[Chain, ValidationErrors](a))))

  implicit def x[A: Eq]: Eq[EitherT[ValidationT[Chain, ?], Throwable, A]] = Eq.by(_.value)

  implicit def iso: Isomorphisms[ValidationT[Chain, ?]] = Isomorphisms.invariant[ValidationT[Chain, ?]]

  checkAll("validation", MonadErrorTests[ValidationT[Chain, ?], Throwable].monadError[Int, Int, Int])
}
