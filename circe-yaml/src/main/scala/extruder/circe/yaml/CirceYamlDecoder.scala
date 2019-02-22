package extruder.circe.yaml

import extruder.core.Decode
import extruder.data.Validation
import io.circe.Json

trait CirceYamlDecoder extends Decode { self: CirceYamlDataSource =>
  override type InputData = String
  override type DecodeData = Json
  override type DecodeDefault[A] = Validation[A]
}
