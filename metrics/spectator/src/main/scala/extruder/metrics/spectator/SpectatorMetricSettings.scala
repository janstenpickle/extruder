package extruder.metrics.spectator

import com.netflix.spectator.api.{Registry, Spectator}
import extruder.metrics.dimensional.DimensionalMetricSettings

trait SpectatorMetricSettings extends DimensionalMetricSettings {
  val registry: Registry = Spectator.globalRegistry()
}
