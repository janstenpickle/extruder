package extruder.core

import cats.FlatMap
import cats.syntax.flatMap._
import cats.syntax.functor._
import extruder.data.PathElement
import shapeless.{Lazy, Refute}

trait CombinedDecoderTInstances {

  implicit def combinedDecoder[F[_], A, S0, S1, I0, I1](
    implicit ev0: Lazy[DecoderT[F, S0, A, I0]],
    ev1: Lazy[DecoderT[F, S1, A, I1]],
    errors: ExtruderErrors[F],
    refute: Refute[CombinedRefute[A]]
  ): DecoderT[F, (S0, S1), A, (I0, I1)] =
    new DecoderT[F, (S0, S1), A, (I0, I1)] {
      override def read(path: List[PathElement], settings: (S0, S1), default: Option[A], input: (I0, I1)): F[A] =
        errors.fallback(ev0.value.read(path, settings._1, default, input._1))(
          ev1.value.read(path, settings._2, default, input._2)
        )
    }

  implicit def combinedOptionalDecoder[F[_]: FlatMap, A, S0, S1, I0, I1](
    implicit ev0: Lazy[DecoderT[F, S0, Option[A], I0]],
    ev1: Lazy[DecoderT[F, S1, Option[A], I1]],
    errors: ExtruderErrors[F]
  ): DecoderT[F, (S0, S1), Option[A], (I0, I1)] = DecoderT.make { (path, settings, default, input) =>
    lazy val decoded0 = ev0.value.read(path, settings._1, default, input._1)
    lazy val decoded1 = ev1.value.read(path, settings._2, default, input._2)

    lazy val both = for {
      d0 <- decoded0
      d1 <- decoded1
    } yield
      (d0, d1) match {
        case (Some(_), _) => d0
        case (None, Some(_)) => d1
        case (None, None) => None
      }

    errors.fallback(both)(errors.fallback(decoded0)(decoded1))
  }
}
