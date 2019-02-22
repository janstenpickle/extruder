package extruder.circe.yaml

import cats.Applicative
import extruder.core.Transform
import io.circe.Json
import io.circe.yaml.printer

trait CirceYamlFinalizeInstances {
  implicit def circeYamlFinalize[F[_]: Applicative, S]: Transform[F, S, Json, String] =
    Transform.inputBy[F, S, Json, String](printer.print)
}
