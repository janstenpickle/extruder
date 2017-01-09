package shapelessconfig.core

import cats.syntax.validated._
import shapelessconfig.syntax.validation.ConfigValidation

import scala.collection.JavaConverters._

object SystemPropertiesResolvers extends Resolvers {
  val props: Map[String, String] = System.getProperties.asScala.toMap.map { case (k, v) => k.toLowerCase -> v }

  implicit def mkPath(path: Seq[String]): String = pathToString(path)

  override def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]] = props.get(path).validNel
  override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
}