package extruder.metrics.dimensional
import extruder.metrics.data.{MetricType, Numbers}

case class DimensionalMetric private[dimensional] (
  name: String,
  labelNames: Vector[String],
  metricType: MetricType,
  values: Map[Vector[String], Numbers]
)
