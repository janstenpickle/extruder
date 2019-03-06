package extruder.system.systemproperties

import java.util.Properties

import extruder.core.Decode
import extruder.data.Validation
import extruder.map.MapDataSource

trait SystemPropertiesDecoder extends Decode { self: MapDataSource =>
  override type InputData = Properties
  override type DecodeData = Map[String, String]
  override type DecodeDefault[A] = Validation[A]
}
