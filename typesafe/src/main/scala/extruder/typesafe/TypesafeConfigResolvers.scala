package extruder.typesafe

import cats.syntax.either._
import cats.syntax.validated._
import com.typesafe.config.ConfigException.Missing
import com.typesafe.config.{Config, ConfigList, ConfigObject, ConfigValue}
import extruder.core._

import scala.collection.JavaConverters._

case class TypesafeConfigResolvers(config: Config) extends Resolvers {
  def lookup[T](f: Config => T, path: Seq[String]): ConfigValidation[Option[T]] = Either.catchNonFatal(f(config)).fold({
    case _ : Missing => None.validNel
    case th: Any => ValidationFailure(
      s"Could not retrieve config '${pathToString(path)}' from supplied Typesafe config",
      Some(th)
    )
  }, Some(_).validNel)

  override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase

  override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] =
    lookup(_.getString(pathToString(path)), path)

  override def lookupList(path: Seq[String]): ConfigValidation[Option[List[String]]] =
    lookup(_.getStringList(pathToString(path)).asScala.toList, path)

  def resolve[T](lookup: Seq[String] => ConfigValidation[Option[T]]): (Seq[String], Option[T]) => ConfigValidation[T] =
    resolve[T, T](_.validNel, lookup)

  implicit val configValueResolver: Resolver[ConfigValue] =
    Resolver(resolve[ConfigValue](path => lookup(_.getValue(pathToString(path)), path)))

  implicit val configListResolver: Resolver[ConfigList] =
    Resolver(resolve[ConfigList](path => lookup(_.getList(pathToString(path)), path)))

  implicit val configObjectResolver: Resolver[ConfigObject] =
    Resolver(resolve[ConfigObject](path => lookup(_.getObject(pathToString(path)), path)))
}
