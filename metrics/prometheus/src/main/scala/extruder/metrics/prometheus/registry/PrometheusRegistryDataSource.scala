package extruder.metrics.prometheus.registry

import extruder.core.DataSource

trait PrometheusRegistryDataSource extends DataSource {
  override type Sett = PrometheusRegistryMetricSettings
  override val defaultSettings: PrometheusRegistryMetricSettings = new PrometheusRegistryMetricSettings {}
}
