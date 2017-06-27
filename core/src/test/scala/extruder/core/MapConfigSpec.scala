package extruder.core
import cats.effect.IO
import cats.kernel.laws.GroupLaws
import cats.instances.all._

class MapConfigSpec extends ConfigSpec with MapEncoders with MapDecoders with DecodeFromDefaultConfig {
  override def convertConfig(map: Map[Seq[String], String])(implicit utils: Hint): Map[String, String] =
    map.map { case (k, v) => utils.pathToString(k) -> v }

  override def monoidGroupLaws: GroupLaws[Map[String, String]] = GroupLaws[Map[String, String]]

  override implicit def utils: MapHints = MapHints.default

  override def loadConfig: IO[InputConfig] = IO(convertConfig(caseClassConfig))
}
