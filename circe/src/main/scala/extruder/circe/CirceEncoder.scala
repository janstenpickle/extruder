package extruder.circe

import cats.Id
import extruder.core.Encode
import io.circe.Json

trait CirceEncoder extends Encode { self: CirceDataSource =>
  override type EncodeData = Json
  override type OutputData = Json
  override type EncodeDefault[A] = Id[A]
}
