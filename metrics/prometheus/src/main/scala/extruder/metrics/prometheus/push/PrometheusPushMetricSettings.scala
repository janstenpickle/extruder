package extruder.metrics.prometheus.push

import extruder.metrics.dimensional.DimensionalMetricSettings
import io.prometheus.client.exporter.PushGateway

trait PrometheusPushMetricSettings extends DimensionalMetricSettings {
  def pushGateway: PushGateway = new PushGateway("127.0.0.1:9091")
  def jobName: String = "extruder"
  def jobInstance: String = "extruder"
}
