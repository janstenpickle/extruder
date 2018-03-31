package extruder.metrics.dimensional

import cats.Eq
import cats.instances.int._
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.ScalacheckShapeless._
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class ResetNamespaceSpec extends Specification with Discipline {
  import ResetNamespaceSpec._

  override def is: SpecStructure =
    checkAll("Metric values monoid", MonoidTests[ResetNamespace[Int]].monoid)
}

object ResetNamespaceSpec {
  implicit def resetNamespaceEq[A: Eq]: Eq[ResetNamespace[A]] = Eq.by(_.value)
}
