package extruder.metrics.data

import cats.Eq
import cats.instances.all._
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.ScalacheckShapeless._
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class MetricValuesSpec extends Specification with Discipline {
  import MetricValuesSpec._

  override def is: SpecStructure =
    checkAll("Metric values monoid", MonoidTests[MetricValues[CounterValue, Int]].monoid)
}

object MetricValuesSpec {
  implicit val metricValuesEq: Eq[MetricValues[CounterValue, Int]] = Eq.by(_.toString)
}
