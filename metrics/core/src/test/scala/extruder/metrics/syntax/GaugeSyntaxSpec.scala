package extruder.metrics.syntax

import extruder.metrics.data.GaugeValue
import extruder.metrics.syntax.gauge._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class GaugeSyntaxSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  test("Can convert a numeric into a gauge")(forAll { i: Int =>
    i.toGauge === GaugeValue(i)
  })
}
