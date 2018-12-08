package extruder.metrics.dimensional

import extruder.metrics.data.MetricType
import extruder.metrics.{snakeCaseTransformation, MetricSettings}

trait DimensionalMetricSettings extends MetricSettings {
  def labelTransform(value: String): String = snakeCaseTransformation(value)
  def namespaceName: Option[String] = None
  def defaultLabels: Map[String, String] = Map.empty
}
