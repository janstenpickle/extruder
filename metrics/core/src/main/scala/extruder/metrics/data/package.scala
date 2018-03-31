package extruder.metrics

import shapeless.{:+:, CNil}

package object data {
  type CounterValues[A] = MetricValues[CounterValue, A]
  type GaugeValues[A] = MetricValues[GaugeValue, A]
  type TimerValues = MetricValues[TimerValue, Long]

  type Numbers = Short :+: Int :+: Long :+: Float :+: Double :+: CNil
  type Metrics = Map[MetricKey, Numbers]
}
