package extruder.core

import cats.syntax.either._
import cats.syntax.validated._

import scala.collection.JavaConverters._

trait SystemPropertiesDecoder[T] extends Decoder[T, Map[String, String]]

trait SystemPropertiesDecoders extends BaseMapDecoders with Decode[java.util.Properties, Map[String, String], MapDecoder] {
  override protected def prepareConfig(config: java.util.Properties): ConfigValidation[Map[String, String]] =
    config.asScala.toMap.map{ case (k, v) => (k.toLowerCase, v) }.validNel

  def decode[T](implicit decoder: MapDecoder[T]): ConfigValidation[T] =
    decode[T](System.getProperties)
}

object SystemPropertiesDecoder extends SystemPropertiesDecoders

trait SystemPropertiesEncoder[T] extends Encoder[T, Map[String, String]]

trait SystemPropertiesEncoders extends BaseMapEncoders with Encode[Map[String, String], Unit, MapEncoder] {
  override protected def finalizeConfig(inter: Map[String, String]): ConfigValidation[Unit] =
    Either.catchNonFatal {
      inter.foreach { case (k, v) => System.setProperty(k, v) }
    }.leftMap(ex => ValidationException(ex.getMessage, ex)).toValidatedNel
}

object SystemPropertiesEncoder extends SystemPropertiesEncoders

trait SystemPropertiesConfig extends SystemPropertiesDecoders with SystemPropertiesEncoders

object SystemPropertiesConfig extends SystemPropertiesConfig
