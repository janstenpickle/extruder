package extruder.instances

import cats.Eq
import cats.instances.string._
import cats.laws.discipline.ContravariantTests
import cats.laws.discipline.eq._
import extruder.core.Show
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ShowInstancesSuite extends FunSuite with Discipline {
  import ShowInstancesSuite._

  checkAll("Show", ContravariantTests[Show].contravariant[Int, Int, Int])
}

object ShowInstancesSuite {
  implicit def faAb[A]: Arbitrary[Show[A]] =
    Arbitrary(Show[A] { a: A =>
      a.toString
    })

  implicit def faEq[A: Arbitrary]: Eq[Show[A]] =
    Eq.by[Show[A], A => String] { showInstance => (a: A) =>
      showInstance.show(a)
    }
}
