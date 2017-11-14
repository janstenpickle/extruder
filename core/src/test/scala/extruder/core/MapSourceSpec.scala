package extruder.core
import cats.instances.all._
import cats.kernel.laws.GroupLaws
import extruder.effect.ExtruderAsync

class MapSourceSpec extends SourceSpec with MapEncoders with MapDecoders with DecodeFromDefaultSource {
  override def convertData(map: Map[List[String], String])(implicit hints: Hint): Map[String, String] =
    map.map { case (k, v) => hints.pathToString(k) -> v }

  override def monoidGroupLaws: GroupLaws[Map[String, String]] = GroupLaws[Map[String, String]]

  override implicit def hints: MapHints = MapHints.default

  override def loadInput[F[_]](implicit F: ExtruderAsync[F]): F[InputData] = F.pure(convertData(caseClassData))
}
