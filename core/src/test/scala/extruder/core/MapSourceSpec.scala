package extruder.core
import cats.instances.all._
import cats.kernel.laws.discipline.MonoidTests

class MapSourceSpec extends StringMapSourceSpec with MapEncoders with MapDecoders with DecodeFromDefaultSource {
  override def convertData(map: Map[List[String], String])(implicit hints: Hint): Map[String, String] =
    map.map { case (k, v) => hints.pathToString(k) -> v }

  override def monoidTests: MonoidTests[Map[String, String]]#RuleSet = MonoidTests[Map[String, String]].monoid

  override implicit def hints: MapHints = MapHints.default

  override def loadInput[F[_]](implicit F: Eff[F]): F[InputData] = F.pure(convertData(caseClassData))
}
