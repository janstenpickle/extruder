package extruder.map

import cats.Id
import extruder.core.Encode

trait MapEncoder extends Encode { self: MapDataSource =>
  override type EncodeData = Map[String, String]
  override type OutputData = Map[String, String]
  override type EncodeDefault[A] = Id[A]
}
