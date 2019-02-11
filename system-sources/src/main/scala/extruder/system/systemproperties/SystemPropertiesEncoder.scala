package extruder.system.systemproperties

import extruder.core.Encode
import extruder.data.Validation
import extruder.map.MapDataSource

trait SystemPropertiesEncoder extends Encode { self: MapDataSource =>
  override type EncodeData = Map[String, String]
  override type OutputData = Unit
  override type EncodeDefault[A] = Validation[A]
}
