package extruder.metrics.keyed

import extruder.metrics.data.{MetricType, Numbers}

case class KeyedMetric private[keyed] (name: String, metricType: MetricType, value: Numbers)
