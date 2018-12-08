package extruder.metrics.spectator

import com.netflix.spectator.api.{Registry, Spectator}
import extruder.core.DataSource

trait SpectatorDataSource extends DataSource {
  override type Sett = SpectatorMetricSettings

  override def defaultSettings: SpectatorMetricSettings = new SpectatorMetricSettings {}
}
