package extruder.metrics.dropwizard.keyed

import extruder.core.DataSource

trait DropwizardKeyedDataSource extends DataSource {
  override type Sett = DropwizardKeyedMetricSettings
  override val defaultSettings: DropwizardKeyedMetricSettings = new DropwizardKeyedMetricSettings {}
}
