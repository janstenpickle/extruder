package extruder.core
import cats.effect.IO
import cats.kernel.laws.GroupLaws
import cats.instances.all._

class MapSourceSpec extends SourceSpec with MapEncoders with MapDecoders with DecodeFromDefaultSource {
  override def convertData(map: Map[List[String], String])(implicit hints: Hint): Map[String, String] =
    map.map { case (k, v) => hints.pathToString(k) -> v }

  override def monoidGroupLaws: GroupLaws[Map[String, String]] = GroupLaws[Map[String, String]]

  override implicit def hints: MapHints = MapHints.default

  override def loadInput: IO[InputData] = IO(convertData(caseClassData))
}
