package extruder.core

import cats.syntax.validated._

class MapResolvers(config: Map[String, String]) extends Resolvers {
  lazy val lowerCaseKeysConfig: Map[String, String] = config.map { case (k, v) => k.toLowerCase -> v }

  override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] =
    lowerCaseKeysConfig.get(pathToString(path)).validNel

  override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
}

object MapResolvers {
  def apply(map: Map[String, String]): MapResolvers = new MapResolvers(map)
}
