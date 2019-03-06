package extruder.typesafe

import extruder.core.Encode
import extruder.typesafe.IntermediateTypes.Config
import com.typesafe.config.{Config => TConfig}
import extruder.data.Validation

trait TypesafeConfigEncoder extends Encode { self: TypesafeConfigDataSource =>
  override type EncodeData = Config
  override type OutputData = TConfig
  override type EncodeDefault[A] = Validation[A]
}
