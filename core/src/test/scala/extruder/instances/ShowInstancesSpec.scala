package extruder.instances

import cats.Eq
import cats.instances.string._
import cats.laws.discipline.ContravariantTests
import cats.laws.discipline.eq._
import extruder.core.Show
import org.scalacheck.Arbitrary
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class ShowInstancesSpec extends Specification with Discipline {
  import ShowInstancesSpec._

  override def is: SpecStructure =
    s2"""
        ${checkAll("Show", ContravariantTests[Show].contravariant[Int, Int, Int])}
      """
}

object ShowInstancesSpec {
  implicit def faAb[A]: Arbitrary[Show[A]] =
    Arbitrary(Show[A] { a: A =>
      a.toString
    })

  implicit def faEq[A: Arbitrary]: Eq[Show[A]] =
    Eq.by[Show[A], A => String] { showInstance => (a: A) =>
      showInstance.show(a)
    }
}
