package extruder.instances

import cats.Eq
import cats.instances.list._
import cats.instances.map._
import cats.instances.string._
import cats.laws.discipline.ContravariantTests
import cats.laws.discipline.eq._
import extruder.core.MultiShow
import org.scalacheck.Arbitrary
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

class MultiShowInstancesSpec extends Specification with ScalaCheck with Discipline {
  import MultiShowInstancesSpec._

  override def is: SpecStructure =
    s2"""
        ${checkAll("MultiShow", ContravariantTests[MultiShow].contravariant[Int, Int, Int])}
      """
}

object MultiShowInstancesSpec {
  implicit def faAb[A]: Arbitrary[MultiShow[A]] =
    Arbitrary(new MultiShow[A] {
      override def show(v: A): Map[List[String], String] = Map(List("test") -> v.toString)
    })

  implicit def faEq[A: Arbitrary]: Eq[MultiShow[A]] =
    Eq.by[MultiShow[A], A => Map[List[String], String]] { showInstance => (a: A) =>
      showInstance.show(a)
    }
}
