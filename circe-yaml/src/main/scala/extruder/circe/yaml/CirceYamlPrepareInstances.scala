package extruder.circe.yaml

import cats.Monad
import extruder.core.{ExtruderErrors, Transform}
import io.circe.Json
import io.circe.yaml.parser

trait CirceYamlPrepareInstances {
  implicit def circeYamlPrepare[F[_]: Monad, S](implicit error: ExtruderErrors[F]): Transform[F, S, String, Json] =
    Transform.inputByF(input => error.fromEitherThrowable(parser.parse(input)))
}
