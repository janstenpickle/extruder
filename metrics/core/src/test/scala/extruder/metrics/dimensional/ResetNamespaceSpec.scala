package extruder.metrics.dimensional

import cats.Eq
import cats.instances.int._
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ResetNamespaceSpec extends FunSuite with Discipline {
  import ResetNamespaceSpec._

  checkAll("Metric values monoid", MonoidTests[ResetNamespace[Int]].monoid)
}

object ResetNamespaceSpec {
  implicit def resetNamespaceEq[A: Eq]: Eq[ResetNamespace[A]] = Eq.by(_.value)
}
