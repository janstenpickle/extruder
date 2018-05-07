package extruder.metrics.syntax

import extruder.metrics.data.CounterValue

trait CounterSyntax {
  implicit class NumericCounterOps[T: Numeric](t: T) {
    def toCounter: CounterValue[T] = CounterValue(t)
  }
}
