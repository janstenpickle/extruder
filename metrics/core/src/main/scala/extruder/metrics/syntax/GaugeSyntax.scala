package extruder.metrics.syntax

import extruder.metrics.data.GaugeValue

trait GaugeSyntax {
  implicit class NumericGaugeOps[T: Numeric](t: T) {
    def toGauge: GaugeValue[T] = GaugeValue(t)
  }
}
