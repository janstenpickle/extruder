package extruder.system.systemproperties

import java.util.Properties

import cats.{Applicative, MonadError}
import extruder.core.Settings
import extruder.data.{LoadInput, Transform}
import extruder.map.MapDecoderInstances

import scala.collection.JavaConverters._

trait SystemPropertiesDecoderInstances extends MapDecoderInstances {
  implicit def systemPropertiesDecoderLoadInput[F[_]](implicit F: MonadError[F, Throwable]): LoadInput[F, Properties] =
    new LoadInput[F, Properties] {
      override def load: F[Properties] = F.catchNonFatal(System.getProperties)
    }

  implicit def systemPropertiesDecoderPrepareInput[F[_]: Applicative]: Transform[
    F,
    Settings,
    Properties,
    Map[String, String]
  ] = Transform.inputBy[F, Settings, Properties, Map[String, String]](_.asScala.toMap)

}
