package extruder.metrics.prometheus.push

import extruder.core.DataSource

trait PrometheusPushDataSource extends DataSource {
  override type Sett = PrometheusPushMetricSettings
  override val defaultSettings: PrometheusPushMetricSettings = new PrometheusPushMetricSettings {}
}
