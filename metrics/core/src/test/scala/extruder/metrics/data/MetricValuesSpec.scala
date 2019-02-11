package extruder.metrics.data

import cats.Eq
import cats.instances.all._
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class MetricValuesSpec extends FunSuite with Discipline {
  import MetricValuesSpec._
  checkAll("Metric values monoid", MonoidTests[MetricValues[CounterValue, String, Int]].monoid)
}

object MetricValuesSpec {
  implicit def metricValuesEq[A]: Eq[MetricValues[CounterValue, A, Int]] = Eq.by(_.toString)
}
