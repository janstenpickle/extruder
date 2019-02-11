package extruder.map

import extruder.core.Decode
import extruder.data.Validation

trait MapDecoder extends Decode { self: MapDataSource =>
  override type InputData = Map[String, String]
  override type DecodeData = Map[String, String]
  override type DecodeDefault[A] = Validation[A]
}
