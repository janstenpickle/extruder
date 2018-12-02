package extruder.system

import java.util.Properties

import cats.MonadError
import cats.effect.Sync
import extruder.cats.effect.EvalValidation
import extruder.core._
import extruder.data.Validation

import scala.collection.JavaConverters._

trait SystemPropertiesCommon { self: DataSource =>
  override val defaultSettings: Settings = new Settings {
    override def pathToString(path: List[String]): String = path.mkString(".").toLowerCase
  }
}

trait SystemPropertiesDecoders
    extends SystemPropertiesCommon
    with BaseMapDecoders
    with Decode
    with DecodeFromDefaultSource
    with DecodeTypes {
  override type InputData = java.util.Properties
  override type DecEff[F[_]] <: MonadError[F, Throwable]

  override protected def prepareInput[F[_]](namespace: List[String], settings: Settings, data: java.util.Properties)(
    implicit F: DecEff[F]
  ): F[Map[String, String]] =
    F.pure(data.asScala.toMap)

  override def loadInput[F[_]](implicit F: DecEff[F]): F[Properties] = F.catchNonFatal(System.getProperties)
}

object SystemPropertiesDecoder extends SystemPropertiesDecoders {
  override type DecDefault[A] = Validation[A]
}

trait SystemPropertiesEncoders extends SystemPropertiesCommon with BaseMapEncoders with Encode {
  override type OutputData = Unit
  override type EncEff[F[_]] <: MonadError[F, Throwable]

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Settings, inter: Map[String, String])(
    implicit F: EncEff[F]
  ): F[Unit] =
    inter
      .map { case (k, v) => F.catchNonFatal(System.setProperty(k, v)) }
      .foldLeft(F.pure(()))((acc, v) => F.ap2(F.pure((_: Unit, _: String) => ()))(acc, v))
}

object SystemPropertiesEncoder extends SystemPropertiesEncoders {
  override type EncDefault[A] = Validation[A]
}

trait SystemPropertiesSource extends SystemPropertiesDecoders with SystemPropertiesEncoders {
  override type DecDefault[A] = Validation[A]
  override type EncDefault[A] = Validation[A]
  override type DecEff[F[_]] = MonadError[F, Throwable]
}

object SystemPropertiesSource extends SystemPropertiesSource

trait SafeSystemPropertiesSource extends SystemPropertiesDecoders with SystemPropertiesEncoders {
  override type DecDefault[A] = EvalValidation[A]
  override type EncDefault[A] = EvalValidation[A]
  override type DecEff[F[_]] = Sync[F]
  override type EncEff[F[_]] = Sync[F]
  override def loadInput[F[_]](implicit F: DecEff[F]): F[Properties] = F.delay(System.getProperties)

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Settings, inter: Map[String, String])(
    implicit F: EncEff[F]
  ): F[Unit] =
    inter
      .map { case (k, v) => F.delay(System.setProperty(k, v)) }
      .foldLeft(F.pure(()))((acc, v) => F.ap2(F.pure((_: Unit, _: String) => ()))(acc, v))
}

object SafeSystemPropertiesSource extends SafeSystemPropertiesSource
