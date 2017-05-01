package extruder.system

import cats.syntax.validated._
import extruder.core._

import scala.collection.JavaConverters._

trait EnvironmentDecoder[T] extends Decoder[T, Map[String, String]]

trait EnvironmentDecoders extends Decoders[Map[String, String], EnvironmentDecoder]
                          with PrimitiveDecoders[Map[String, String], EnvironmentDecoder]
                          with DerivedDecoders[Map[String, String], EnvironmentDecoder]
                          with Decode[java.util.Map[String, String], Map[String, String], EnvironmentDecoder]
                          with ResolutionCommon {

  override protected def pathToString(path: Seq[String]): String = path.mkString("_").toUpperCase

  override protected def lookupValue(path: Seq[String], config: Map[String, String]): ConfigValidation[Option[String]] =
    config.get(pathToString(path)).validNel

  override protected def mkDecoder[T](f: (Seq[String], Option[T], Map[String, String]) => ConfigValidation[T]): EnvironmentDecoder[T] =
    new EnvironmentDecoder[T] {
      override def read(path: Seq[String], default: Option[T], config: Map[String, String]): ConfigValidation[T] = f(path, default, config)
    }

  override protected def prepareConfig(config: java.util.Map[String, String]): ConfigValidation[Map[String, String]] =
    config.asScala.toMap.map { case (k, v) => k.toUpperCase -> v }.validNel

  def decode[T](implicit decoder: EnvironmentDecoder[T]): ConfigValidation[T] =
    decode[T](Seq.empty)

  def decode[T](namespace: Seq[String])(implicit decoder: EnvironmentDecoder[T]): ConfigValidation[T] =
    decode[T](namespace, System.getenv())
}

object EnvironmentDecoder extends EnvironmentDecoders

object EnvironmentConfig extends EnvironmentDecoders
