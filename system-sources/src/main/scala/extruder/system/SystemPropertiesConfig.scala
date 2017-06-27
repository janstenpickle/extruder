package extruder.system

import java.util.Properties

import cats.effect.IO
import cats.syntax.cartesian._
import extruder.core._

import scala.collection.JavaConverters._

trait SystemPropertiesDecoders extends BaseMapDecoders with Decode with DecodeFromDefaultConfig with DecodeTypes {
  override type InputConfig = java.util.Properties
  override type OutputConfig = Unit

  override protected def prepareConfig[F[_], E](
    namespace: Seq[String],
    config: java.util.Properties
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(config.asScala.toMap.map { case (k, v) => k.toLowerCase -> v }))

  override def loadConfig: IO[Properties] = IO(System.getProperties)
}

object SystemPropertiesDecoder extends SystemPropertiesDecoders

trait SystemPropertiesEncoders extends BaseMapEncoders with Encode {
  override type OutputConfig = Unit

  override protected def finalizeConfig[F[_], E](
    namespace: Seq[String],
    inter: Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Unit]] = IO.pure {
    inter
      .map { case (k, v) => AE.catchNonFatal(System.setProperty(k, v)) }
      .foldLeft(AE.pure(()))((acc, v) => (acc |@| v).map((_, _) => ()))
  }
}

object SystemPropertiesEncoder extends SystemPropertiesEncoders

trait SystemPropertiesConfig extends SystemPropertiesDecoders with SystemPropertiesEncoders {
  override type Hint = MapHints
}

object SystemPropertiesConfig extends SystemPropertiesConfig
