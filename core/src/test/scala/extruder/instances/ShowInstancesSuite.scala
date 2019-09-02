package extruder.instances

import cats.Eq
import cats.instances.string._
import cats.laws.discipline.eq._
import cats.laws.discipline.{ContravariantTests, ExhaustiveCheck}
import extruder.CoreTestInstances._
import extruder.core.Show
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline

class ShowInstancesSuite extends AnyFunSuite with Discipline {
  import ShowInstancesSuite._

  checkAll("Show", ContravariantTests[Show].contravariant[Int, Int, Int])
}

object ShowInstancesSuite {
  implicit def faAb[A]: Arbitrary[Show[A]] =
    Arbitrary(Show.make[A](_.toString))

  implicit def faEq[A: Arbitrary: ExhaustiveCheck]: Eq[Show[A]] =
    Eq.by[Show[A], A => String] { showInstance => (a: A) =>
      showInstance.show(a)
    }
}
