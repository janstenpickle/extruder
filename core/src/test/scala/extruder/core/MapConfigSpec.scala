package extruder.core
import cats.kernel.laws.GroupLaws
import cats.instances.all._

class MapConfigSpec extends ConfigSpec[Map[String, String], Map[String, String], Map[String, String], MapDecoder, MapEncoder]
                    with MapEncoders
                    with MapDecoders {
  override def convertConfig(map: Map[String, String]): Map[String, String] = map
  override def monoidGroupLaws: GroupLaws[Map[String, String]] = GroupLaws[Map[String, String]]
}