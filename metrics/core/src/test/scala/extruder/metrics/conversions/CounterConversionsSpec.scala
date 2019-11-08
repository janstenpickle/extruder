package extruder.metrics.conversions

import extruder.metrics.conversions.counter._
import extruder.metrics.data.CounterValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CounterConversionsSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  test("Can convert a numeric into a counter")(forAll { i: Int =>
    val c: CounterValue[Int] = i
    assert(c === CounterValue(i))
  })
}
