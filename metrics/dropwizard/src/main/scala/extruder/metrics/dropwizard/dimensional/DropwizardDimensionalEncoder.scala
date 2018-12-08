package extruder.metrics.dropwizard.dimensional

import extruder.cats.effect.EvalValidation
import extruder.core.Encode
import extruder.metrics.data.Metrics
import io.dropwizard.metrics5.MetricRegistry

trait DropwizardDimensionalEncoder extends Encode { self: DropwizardDimensionalDataSource =>
  override type EncodeData = Metrics
  override type OutputData = MetricRegistry
  override type EncodeDefault[A] = EvalValidation[A]
}
