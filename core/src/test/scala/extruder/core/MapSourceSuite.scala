//package extruder.core
//
//import cats.instances.all._
//import cats.kernel.laws.discipline.MonoidTests
//
//class MapSourceSuite
//    extends StringMapSourceSuite(MonoidTests[Map[String, String]].monoid)
//    with MapEncoders
//    with MapDecoders
//    with DecodeFromDefaultSource {
//  override def convertData(map: Map[List[String], String]): Map[String, String] =
//    map.map { case (k, v) => defaultSettings.pathToString(k) -> v }
//
//  override def loadInput[F[_]](implicit F: DecEff[F]): F[InputData] = F.pure(convertData(caseClassData))
//}
