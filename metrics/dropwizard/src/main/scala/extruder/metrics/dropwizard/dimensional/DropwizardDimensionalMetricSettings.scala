package extruder.metrics.dropwizard.dimensional

import extruder.metrics.dimensional.DimensionalMetricSettings
import io.dropwizard.metrics5.MetricRegistry

trait DropwizardDimensionalMetricSettings extends DimensionalMetricSettings {
  val registry: MetricRegistry = new MetricRegistry()
}
