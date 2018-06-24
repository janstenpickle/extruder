package extruder.system

import extruder.core._
import extruder.effect.{ExtruderMonadError, ExtruderSync}

import scala.collection.JavaConverters._

trait EnvironmentDecoder[F[_], T] extends Decoder[F, Settings, T, Map[String, String]]

trait EnvironmentDecoders extends BaseEnvironmentDecoders {
  override type Eff[F[_]] = ExtruderMonadError[F]
}

trait SafeEnvironmentDecoders extends BaseEnvironmentDecoders {
  override type Eff[F[_]] = ExtruderSync[F]

  override def loadInput[F[_]](implicit F: Eff[F]): F[java.util.Map[String, String]] =
    F.delay(System.getenv())
}

trait BaseEnvironmentDecoders
    extends Decoders
    with PrimitiveDecoders
    with DerivedDecoders
    with Decode
    with DecodeFromDefaultSource
    with DecodeTypes {
  override type InputData = java.util.Map[String, String]
  override type DecodeData = Map[String, String]
  override type Dec[F[_], T] = EnvironmentDecoder[F, T]
  override type Sett = Settings

  override val defaultSettings: Settings = new Settings {
    override def pathToString(path: List[String]): String = path.mkString("_").toUpperCase
  }

  override protected def hasValue[F[_]](path: List[String], settings: Settings, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Boolean] =
    F.map(lookupValue(path, settings, data))(_.isDefined)

  override protected def lookupValue[F[_]](path: List[String], settings: Settings, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Option[String]] =
    F.pure(data.get(settings.pathToString(path)))

  override protected def mkDecoder[F[_], T](
    f: (List[String], Settings, Option[T], Map[String, String]) => F[T]
  ): EnvironmentDecoder[F, T] =
    new EnvironmentDecoder[F, T] {
      override def read(path: List[String], settings: Settings, default: Option[T], data: Map[String, String]): F[T] =
        f(path, settings, default, data)
    }

  override protected def prepareInput[F[_]](
    namespace: List[String],
    settings: Settings,
    data: java.util.Map[String, String]
  )(implicit F: Eff[F]): F[Map[String, String]] =
    F.pure(data.asScala.toMap)

  override def loadInput[F[_]](implicit F: Eff[F]): F[java.util.Map[String, String]] =
    F.catchNonFatal(System.getenv())
}

object EnvironmentDecoder extends EnvironmentDecoders

object EnvironmentSource extends EnvironmentDecoders

object SafeEnvironmentSource extends SafeEnvironmentDecoders
