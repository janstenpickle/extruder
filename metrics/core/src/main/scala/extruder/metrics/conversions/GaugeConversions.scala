package extruder.metrics.conversions

import extruder.metrics.data.GaugeValue

trait GaugeConversions {
  implicit def numericToGauge[A: Numeric](a: A): GaugeValue[A] = GaugeValue(a)
}
