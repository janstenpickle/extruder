package extruder.metrics

import shapeless.{:+:, CNil}

package object data {
  type CounterValues[K, V] = MetricValues[CounterValue, K, V]
  type GaugeValues[K, V] = MetricValues[GaugeValue, K, V]
  type TimerValues[K] = MetricValues[TimerValue, K, Long]

  type Numbers = Short :+: Int :+: Long :+: Float :+: Double :+: CNil
  type Metrics = Map[MetricKey, Numbers]
}
