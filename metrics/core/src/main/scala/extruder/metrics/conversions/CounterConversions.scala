package extruder.metrics.conversions

import extruder.metrics.data.CounterValue

trait CounterConversions {
  implicit def numericToCounter[A: Numeric](a: A): CounterValue[A] = CounterValue(a)
}
