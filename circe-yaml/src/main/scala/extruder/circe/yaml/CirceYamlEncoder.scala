package extruder.circe.yaml

import cats.Id
import extruder.core.Encode
import io.circe.Json

trait CirceYamlEncoder extends Encode { self: CirceYamlDataSource =>
  override type EncodeData = Json
  override type OutputData = String
  override type EncodeDefault[A] = Id[A]
}
