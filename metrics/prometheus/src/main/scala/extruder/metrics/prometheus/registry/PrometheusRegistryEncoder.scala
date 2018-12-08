package extruder.metrics.prometheus.registry

import extruder.cats.effect.EvalValidation
import extruder.core.Encode
import extruder.metrics.data.Metrics
import io.prometheus.client.CollectorRegistry

trait PrometheusRegistryEncoder extends Encode { self: PrometheusRegistryDataSource =>
  override type EncodeData = Metrics
  override type OutputData = CollectorRegistry
  override type EncodeDefault[A] = EvalValidation[A]
}
