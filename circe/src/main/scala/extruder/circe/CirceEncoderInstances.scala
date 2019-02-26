package extruder.circe

import cats.{Applicative, Monad}
import cats.syntax.applicative._
import extruder.core.EncoderT
import io.circe.syntax._
import io.circe.{Encoder, Json}
import cats.instances.vector._
import cats.kernel.Monoid
import cats.syntax.traverse._
import extruder.data.PathElement

import scala.collection.generic.CanBuildFrom
import cats.syntax.flatMap._
import shapeless.{<:!<, LowPriority}

trait CirceEncoderInstances {
  implicit val jsonMonoid: Monoid[Json] = new Monoid[Json] {
    override def empty: Json = Json.Null
    override def combine(x: Json, y: Json): Json =
      if (x.isNull) y
      else if (y.isNull) x
      else x.deepMerge(y)
  }

  private def encode[F[_]: Applicative, A: Encoder](path: List[PathElement], settings: CirceSettings, a: A): F[Json] =
    settings.pathElementsAsStrings(path).reverse match {
      case head :: tail =>
        tail
          .foldLeft(Map(settings.formatElement(head) -> a.asJson)) { (acc, elem) =>
            Map(settings.formatElement(elem) -> acc.asJson)
          }
          .asJson
          .pure[F]
      case Nil => a.asJson.pure[F]
    }

  implicit def fromCirceEncoder[F[_]: Applicative, S <: CirceSettings, A: Encoder](
    implicit neOpt: A <:!< Option[_],
    lp: LowPriority
  ): EncoderT[F, S, A, Json] =
    EncoderT.make(encode[F, A])

  implicit def circeObjectArrayEncoder[F[_]: Monad, FF[T] <: TraversableOnce[T], S <: CirceSettings, A](
    implicit encoder: EncoderT[F, S, A, Json]
  ): EncoderT[F, S, FF[A], Json] = EncoderT.make { (path, settings, as) =>
    as.toVector
      .traverse { a =>
        encoder.write(List.empty, settings, a)
      }
      .flatMap(encode[F, Vector[Json]](path, settings, _))
  }
}
