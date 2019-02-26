package extruder.circe

import cats.Applicative
import cats.syntax.applicative._
import extruder.core.{DecoderT, ExtruderErrors, HasValue, Parser}
import extruder.data.PathElement
import io.circe.{Decoder, Json}
import shapeless.{<:!<, LowPriority}
import cats.syntax.traverse._
import cats.instances.vector._
import cats.syntax.functor._

import scala.collection.generic.CanBuildFrom

trait CirceDecoderInstances {
  implicit def circeHasValue[F[_]: Applicative, S <: CirceSettings]: HasValue[F, S, Json] = new HasValue[F, S, Json] {
    override def hasValue(path: List[PathElement], settings: S, data: Json): F[Boolean] =
      focus(path, settings, data).isDefined.pure[F]
  }

  implicit def fromCirceDecoder[F[_]: Applicative, S <: CirceSettings, A: Decoder](
    implicit error: ExtruderErrors[F],
    neOpt: A <:!< Option[_],
    lp: LowPriority
  ): DecoderT[F, S, A, Json] = DecoderT.make[F, S, A, Json] { (path, settings, default, json) =>
    (focus(path, settings, json), default) match {
      case (None, Some(d)) => d.pure[F]
      case (Some(js), _) => error.fromEitherThrowable(js.as[A])
      case (None, None) =>
        error.missing(s"Could not find value at '${settings.pathElementListToString(path)}' and no default available")
    }
  }

  implicit def circeObjectArrayDecoder[F[_]: Applicative, FF[T] <: TraversableOnce[T], S <: CirceSettings, A](
    implicit error: ExtruderErrors[F],
    decoder: DecoderT[F, S, A, Json],
    cbf: CanBuildFrom[FF[A], A, FF[A]]
  ): DecoderT[F, S, FF[A], Json] = DecoderT.make { (path, settings, default, json) =>
    (focus(path, settings, json).flatMap(_.asArray), default) match {
      case (None, Some(d)) => d.pure[F]
      case (Some(js), _) =>
        js.traverse[F, A] { j =>
            decoder.read(List.empty, settings, None, j)
          }
          .map(Parser.convertTraversable(_))
      case (None, None) =>
        error.missing(s"Could not find value at '${settings.pathElementListToString(path)}' and no default available")
    }
  }

  private def focus[S <: CirceSettings](path: List[PathElement], settings: S, json: Json): Option[Json] =
    settings.pathElementsAsStrings(path) match {
      case head :: tail =>
        tail
          .foldLeft(json.hcursor.downField(settings.formatElement(head))) { (acc, elem) =>
            acc.downField(settings.formatElement(elem))
          }
          .focus
      case Nil => Some(json).filterNot(_.isNull)
    }

}
