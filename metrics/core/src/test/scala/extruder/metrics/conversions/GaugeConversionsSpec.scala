package extruder.metrics.conversions

import extruder.metrics.conversions.gauge._
import extruder.metrics.data.GaugeValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class GaugeConversionsSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  test("Can convert a numeric into a gauge")(forAll { i: Int =>
    val c: GaugeValue[Int] = i
    assert(c === GaugeValue(i))
  })
}
