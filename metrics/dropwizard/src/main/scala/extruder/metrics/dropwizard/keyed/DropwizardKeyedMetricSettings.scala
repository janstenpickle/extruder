package extruder.metrics.dropwizard.keyed

import extruder.metrics.MetricSettings
import io.dropwizard.metrics5.MetricRegistry

trait DropwizardKeyedMetricSettings extends MetricSettings {
  val registry: MetricRegistry = new MetricRegistry()
}
