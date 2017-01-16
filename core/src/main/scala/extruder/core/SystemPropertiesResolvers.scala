package extruder.core

import cats.syntax.validated._
import extruder.syntax.validation.ConfigValidation

import scala.collection.JavaConverters._

class SystemPropertiesResolvers extends Resolvers {
  val props: Map[String, String] = System.getProperties.asScala.toMap.map { case (k, v) => k.toLowerCase -> v }

  override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] =
    props.get(pathToString(path)).validNel

  override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
}

object SystemPropertiesResolvers extends SystemPropertiesResolvers