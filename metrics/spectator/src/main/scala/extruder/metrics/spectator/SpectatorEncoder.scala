package extruder.metrics.spectator

import com.netflix.spectator.api.Registry
import extruder.cats.effect.EvalValidation
import extruder.core.Encode
import extruder.metrics.data.Metrics

trait SpectatorEncoder extends Encode { self: SpectatorDataSource =>
  override type EncodeData = Metrics
  override type OutputData = Registry
  override type EncodeDefault[A] = EvalValidation[A]
}
