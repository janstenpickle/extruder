package extruder.system

import java.util.Properties

import extruder.core._
import extruder.effect.{ExtruderMonadError, ExtruderSync}

import scala.collection.JavaConverters._

trait SystemPropertiesDecoders extends BaseMapDecoders with Decode with DecodeFromDefaultSource with DecodeTypes {
  override type InputData = java.util.Properties

  override protected def prepareInput[F[_]](
    namespace: List[String],
    data: java.util.Properties
  )(implicit F: Eff[F], util: Hint): F[Map[String, String]] =
    F.pure(data.asScala.toMap)

  override def loadInput[F[_]](implicit F: Eff[F]): F[Properties] = F.catchNonFatal(System.getProperties)
}

object SystemPropertiesDecoder extends SystemPropertiesDecoders

trait SystemPropertiesEncoders extends BaseMapEncoders with Encode {
  override type OutputData = Unit

  override protected def finalizeOutput[F[_]](
    namespace: List[String],
    inter: Map[String, String]
  )(implicit F: Eff[F], util: Hint): F[Unit] =
    inter
      .map { case (k, v) => F.catchNonFatal(System.setProperty(k, v)) }
      .foldLeft(F.pure(()))((acc, v) => F.ap2(F.pure((_: Unit, _: String) => ()))(acc, v))
}

object SystemPropertiesEncoder extends SystemPropertiesEncoders

trait SystemPropertiesSource extends SystemPropertiesDecoders with SystemPropertiesEncoders {
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Hint = MapHints
}

object SystemPropertiesSource extends SystemPropertiesSource

trait SafeSystemPropertiesSource extends SystemPropertiesDecoders with SystemPropertiesEncoders {
  override type Eff[F[_]] = ExtruderSync[F]
  override type Hint = MapHints
  override def loadInput[F[_]](implicit F: Eff[F]): F[Properties] = F.delay(System.getProperties)

  override protected def finalizeOutput[F[_]](
    namespace: List[String],
    inter: Map[String, String]
  )(implicit F: Eff[F], util: Hint): F[Unit] =
    inter
      .map { case (k, v) => F.delay(System.setProperty(k, v)) }
      .foldLeft(F.pure(()))((acc, v) => F.ap2(F.pure((_: Unit, _: String) => ()))(acc, v))
}

object SafeSystemPropertiesSource extends SafeSystemPropertiesSource
