package extruder.metrics.conversions

import extruder.metrics.conversions.gauge._
import extruder.metrics.data.GaugeValue
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GaugeConversionsSpec extends FunSuite with GeneratorDrivenPropertyChecks {
  test("Can convert a numeric into a gauge")(forAll { i: Int =>
    val c: GaugeValue[Int] = i
    assert(c === GaugeValue(i))
  })
}
