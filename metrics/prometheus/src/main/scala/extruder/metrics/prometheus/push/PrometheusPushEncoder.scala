package extruder.metrics.prometheus.push

import extruder.cats.effect.EvalValidation
import extruder.core.Encode
import extruder.metrics.data.Metrics

trait PrometheusPushEncoder extends Encode { self: PrometheusPushDataSource =>
  override type EncodeDefault[A] = EvalValidation[A]
  override type EncodeData = Metrics
  override type OutputData = Unit
}
