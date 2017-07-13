package extruder.system

import java.util.Properties

import cats.effect.IO
import cats.syntax.cartesian._
import extruder.core._

import scala.collection.JavaConverters._

trait SystemPropertiesDecoders extends BaseMapDecoders with Decode with DecodeFromDefaultSource with DecodeTypes {
  override type InputData = java.util.Properties
  override type OutputData = Unit

  override protected def prepareInput[F[_], E](
    namespace: List[String],
    data: java.util.Properties
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(data.asScala.toMap.map { case (k, v) => k.toLowerCase -> v }))

  override def loadInput: IO[Properties] = IO(System.getProperties)
}

object SystemPropertiesDecoder extends SystemPropertiesDecoders

trait SystemPropertiesEncoders extends BaseMapEncoders with Encode {
  override type OutputData = Unit

  override protected def finalizeOutput[F[_], E](
    namespace: List[String],
    inter: Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Unit]] = IO.pure {
    inter
      .map { case (k, v) => AE.catchNonFatal(System.setProperty(k, v)) }
      .foldLeft(AE.pure(()))((acc, v) => (acc |@| v).map((_, _) => ()))
  }
}

object SystemPropertiesEncoder extends SystemPropertiesEncoders

trait SystemPropertiesSource extends SystemPropertiesDecoders with SystemPropertiesEncoders {
  override type Hint = MapHints
}

object SystemPropertiesSource extends SystemPropertiesSource
