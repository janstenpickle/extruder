package extruder.circe

import cats.Applicative
import cats.syntax.applicative._
import extruder.core.EncoderT
import io.circe.syntax._
import io.circe.{Encoder, Json}

trait CirceEncoderInstances {
  implicit def fromCirceEncoder[F[_]: Applicative, S <: CirceSettings, A: Encoder]: EncoderT[F, S, A, Json] =
    EncoderT.make { (path, settings, a) =>
      lazy val json = a.asJson

      path.reverse match {
        case head :: tail =>
          tail
            .foldLeft(Map(settings.formatElement(head) -> json)) { (acc, elem) =>
              Map(settings.formatElement(elem) -> acc.asJson)
            }
            .asJson
            .pure[F]
        case Nil => json.pure[F]
      }
    }
}
