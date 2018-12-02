package extruder.system

import cats.MonadError
import cats.effect.Sync
import extruder.cats.effect.EvalValidation
import extruder.core._
import extruder.data.Validation

import scala.collection.JavaConverters._

trait EnvironmentDecoder[F[_], T] extends DecoderT[F, Settings, T, Map[String, String]]

trait EnvironmentDecoders extends BaseEnvironmentDecoders {
  override type DecDefault[A] = Validation[A]
  override type DecEff[F[_]] = MonadError[F, Throwable]
}

trait SafeEnvironmentDecoders extends BaseEnvironmentDecoders {
  override type DecDefault[A] = EvalValidation[A]
  override type DecEff[F[_]] = Sync[F]

  override def loadInput[F[_]](implicit F: DecEff[F]): F[java.util.Map[String, String]] =
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
  override type DecT[F[_], T] = EnvironmentDecoder[F, T]
  override type DecEff[F[_]] <: MonadError[F, Throwable]
  override type Sett = Settings

  override val defaultSettings: Settings = new Settings {
    override def pathToString(path: List[String]): String = path.mkString("_").toUpperCase
  }

  override protected def hasValue[F[_]](path: List[String], settings: Settings, data: Map[String, String])(
    implicit F: DecEff[F]
  ): F[Boolean] =
    F.map(lookupValue(path, settings, data))(_.isDefined)

  override protected def lookupValue[F[_]](path: List[String], settings: Settings, data: Map[String, String])(
    implicit F: DecEff[F]
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
  )(implicit F: DecEff[F]): F[Map[String, String]] =
    F.pure(data.asScala.toMap)

  override def loadInput[F[_]](implicit F: DecEff[F]): F[java.util.Map[String, String]] =
    F.catchNonFatal(System.getenv())
}

object EnvironmentDecoder extends EnvironmentDecoders

object EnvironmentSource extends EnvironmentDecoders

object SafeEnvironmentSource extends SafeEnvironmentDecoders
