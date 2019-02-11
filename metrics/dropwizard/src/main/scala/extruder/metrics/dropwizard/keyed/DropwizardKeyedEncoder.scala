package extruder.metrics.dropwizard.keyed

import extruder.cats.effect.EvalValidation
import extruder.core.Encode
import extruder.metrics.data.Metrics
import io.dropwizard.metrics5.MetricRegistry

trait DropwizardKeyedEncoder extends Encode { self: DropwizardKeyedDataSource =>
  override type EncodeData = Metrics
  override type OutputData = MetricRegistry
  override type EncodeDefault[A] = EvalValidation[A]
}
