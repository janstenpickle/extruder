package extruder.metrics.dropwizard.dimensional

import extruder.core.DataSource

trait DropwizardDimensionalDataSource extends DataSource {
  override type Sett = DropwizardDimensionalMetricSettings
  override val defaultSettings: DropwizardDimensionalMetricSettings = new DropwizardDimensionalMetricSettings {}
}
