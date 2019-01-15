package extruder.metrics.spectator

import extruder.core.DataSource

trait SpectatorDataSource extends DataSource {
  override type Sett = SpectatorMetricSettings

  override def defaultSettings: SpectatorMetricSettings = new SpectatorMetricSettings {}
}
