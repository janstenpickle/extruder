package extruder.core
import cats.kernel.laws.GroupLaws
import cats.instances.all._

class MapConfigSpec extends ConfigSpec[Map[String, String], Map[String, String], Map[String, String], MapDecoder, MapEncoder]
                    with MapEncoders
                    with MapDecoders {
  override def convertConfig(map: Map[Seq[String], String]): Map[String, String] =
    map.map { case (k, v) => utils.pathToString(k) -> v }

  override def monoidGroupLaws: GroupLaws[Map[String, String]] = GroupLaws[Map[String, String]]
}