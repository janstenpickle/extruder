package extruder.metrics.syntax

import extruder.metrics.data.GaugeValue
import extruder.metrics.syntax.gauge._
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GaugeSyntaxSpec extends FunSuite with GeneratorDrivenPropertyChecks {
  test("Can convert a numeric into a gauge")(forAll { i: Int =>
    i.toGauge === GaugeValue(i)
  })
}
