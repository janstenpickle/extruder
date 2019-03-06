package extruder.data

import cats.Eq
import cats.data.{Chain, EitherT}
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.MonadErrorTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import extruder.data.ValidationCatsInstances._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.laws.discipline.arbitrary._

class ValidationTSuite extends FunSuite with Discipline {
  implicit def arb[A](implicit arb: Arbitrary[A]) =
    Arbitrary(arb.arbitrary.map(a => ValidationT(EitherT.pure[Chain, ValidationErrors](a))))

  implicit def eitherTEq[A: Eq]: Eq[EitherT[ValidationT[Chain, ?], Throwable, A]] = Eq.by(_.value)

  implicit def iso: Isomorphisms[ValidationT[Chain, ?]] = Isomorphisms.invariant[ValidationT[Chain, ?]]

  implicit def arbF[A]: Arbitrary[A => A] = Arbitrary(Gen.const(identity))

  checkAll("ValidationT", MonadErrorTests[ValidationT[Chain, ?], Throwable].monadError[Int, Int, Int])
  checkAll("ValidationT", EqTests[ValidationT[Chain, Int]].eqv)
}
