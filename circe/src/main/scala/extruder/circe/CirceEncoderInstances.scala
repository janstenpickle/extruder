package extruder.circe

import cats.instances.vector._
import cats.kernel.Monoid
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.{Applicative, Monad}
import extruder.core.Encoder
import extruder.data.PathElement
import io.circe.syntax._
import io.circe.{Json, Encoder => CEncoder}
import shapeless.{<:!<, LowPriority}

import scala.collection.compat._

trait CirceEncoderInstances {
  implicit val jsonMonoid: Monoid[Json] = new Monoid[Json] {
    override def empty: Json = Json.Null
    override def combine(x: Json, y: Json): Json =
      if (x.isNull) y
      else if (y.isNull) x
      else x.deepMerge(y)
  }

  private def encode[F[_]: Applicative, A: CEncoder](path: List[PathElement], settings: CirceSettings, a: A): F[Json] =
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

  implicit def fromCirceEncoder[F[_]: Applicative, S <: CirceSettings, A: CEncoder](
    implicit neOpt: A <:!< Option[_],
    lp: LowPriority
  ): Encoder[F, S, A, Json] =
    Encoder.make(encode[F, A])

  implicit def circeObjectArrayEncoder[F[_]: Monad, FF[T] <: IterableOnce[T], S <: CirceSettings, A](
    implicit encoder: Encoder[F, S, A, Json],
    neOpt: FF[A] <:!< Option[_]
  ): Encoder[F, S, FF[A], Json] = Encoder.make { (path, settings, as) =>
    as.iterator
      .to(Vector)
      .traverse { a =>
        encoder.write(List.empty, settings, a)
      }
      .flatMap(encode[F, Vector[Json]](path, settings, _))
  }
}
