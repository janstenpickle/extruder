package extruder.system.systemproperties

import cats.MonadError
import extruder.core.{Settings, Transform}
import extruder.map.MapEncoderInstances

trait SystemPropertiesEncoderInstances extends MapEncoderInstances {

  implicit def systemPropertiesEncoderTransformOutput[F[_]](
    implicit F: MonadError[F, Throwable]
  ): Transform[F, Settings, Map[String, String], Unit] =
    Transform.inputByF[F, Settings, Map[String, String], Unit](_.map {
      case (k, v) => F.catchNonFatal(System.setProperty(k, v))
    }.foldLeft(F.pure(()))((acc, v) => F.ap2(F.pure((_: Unit, _: String) => ()))(acc, v)))
}
