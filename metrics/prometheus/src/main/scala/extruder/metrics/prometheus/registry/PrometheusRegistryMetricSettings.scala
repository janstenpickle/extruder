package extruder.metrics.prometheus.registry

import java.util.concurrent.ConcurrentHashMap

import extruder.metrics.dimensional.DimensionalMetricSettings
import io.prometheus.client.{CollectorRegistry, Counter, Gauge}

trait PrometheusRegistryMetricSettings extends DimensionalMetricSettings {
  protected[registry] val gauges: ConcurrentHashMap[String, Gauge] = new ConcurrentHashMap[String, Gauge]()
  protected[registry] val counters: ConcurrentHashMap[String, Counter] = new ConcurrentHashMap[String, Counter]()

  val registry: CollectorRegistry = CollectorRegistry.defaultRegistry
}
