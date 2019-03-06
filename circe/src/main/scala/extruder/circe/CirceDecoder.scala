package extruder.circe

import extruder.core.Decode
import extruder.data.Validation
import io.circe.Json

trait CirceDecoder extends Decode { self: CirceDataSource =>
  override type InputData = Json
  override type DecodeData = Json
  override type DecodeDefault[A] = Validation[A]
}
