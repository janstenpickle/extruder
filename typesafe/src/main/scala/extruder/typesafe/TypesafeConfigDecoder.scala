package extruder.typesafe

import com.typesafe.config.Config
import extruder.core.Decode
import extruder.data.Validation

trait TypesafeConfigDecoder extends Decode { self: TypesafeConfigDataSource =>
  override type InputData = Config
  override type DecodeData = Config
  override type DecodeDefault[A] = Validation[A]
}
