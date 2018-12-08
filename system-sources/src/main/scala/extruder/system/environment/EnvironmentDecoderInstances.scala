package extruder.system.environment
import java.util

import cats.{Applicative, MonadError}
import extruder.core.Settings
import extruder.data.{LoadInput, Transform}
import extruder.map.MapDecoderInstances

import scala.collection.JavaConverters._

trait EnvironmentDecoderInstances extends MapDecoderInstances {
  implicit def environmentDecoderLoadInput[F[_]](
    implicit F: MonadError[F, Throwable]
  ): LoadInput[F, util.Map[String, String]] =
    new LoadInput[F, java.util.Map[String, String]] {
      override def load: F[util.Map[String, String]] = F.catchNonFatal(System.getenv())
    }

  implicit def environmentDecoderPrepareInput[F[_]: Applicative]: Transform[
    F,
    Settings,
    util.Map[String, String],
    Map[String, String]
  ] = Transform.inputBy[F, Settings, util.Map[String, String], Map[String, String]](_.asScala.toMap)
}
