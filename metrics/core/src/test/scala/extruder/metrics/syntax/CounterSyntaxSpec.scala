package extruder.metrics.syntax

import extruder.metrics.data.CounterValue
import extruder.metrics.syntax.counter._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CounterSyntaxSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  test("Can convert a numeric into a gauge")(forAll { i: Int =>
    i.toCounter === CounterValue(i)
  })
}
