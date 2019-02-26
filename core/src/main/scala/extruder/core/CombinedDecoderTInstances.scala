package extruder.core

import cats.FlatMap
import cats.data.NonEmptyChain
import cats.syntax.flatMap._
import cats.syntax.functor._
import extruder.data.PathElement
import shapeless.{<:!<, Lazy, Refute}

import scala.collection.TraversableOnce
import scala.collection.generic.CanBuildFrom

trait CombinedDecoderTInstances {

  implicit def combinedDecoder[F[_], A, S0, S1, I0, I1](
    implicit ev0: Lazy[DecoderT[F, S0, A, I0]],
    ev1: Lazy[DecoderT[F, S1, A, I1]],
    errors: ExtruderErrors[F],
    refute: Refute[CombinedRefute[A]],
    neCol: A <:!< TraversableOnce[_],
    neChain: A <:!< NonEmptyChain[_]
  ): DecoderT[F, (S0, S1), A, (I0, I1)] =
    new DecoderT[F, (S0, S1), A, (I0, I1)] {
      override def read(path: List[PathElement], settings: (S0, S1), default: Option[A], input: (I0, I1)): F[A] =
        errors.fallback(ev0.value.read(path, settings._1, default, input._1))(
          ev1.value.read(path, settings._2, default, input._2)
        )
    }

  private def combineOrFallback[F[_]: FlatMap, A, S0, S1, I0, I1](combine: (A, A) => A)(
    path: List[PathElement],
    settings: (S0, S1),
    default: Option[A],
    input: (I0, I1)
  )(implicit ev0: Lazy[DecoderT[F, S0, A, I0]], ev1: Lazy[DecoderT[F, S1, A, I1]], errors: ExtruderErrors[F]): F[A] = {
    lazy val decoded0 = ev0.value.read(path, settings._1, default, input._1)
    lazy val decoded1 = ev1.value.read(path, settings._2, default, input._2)

    lazy val both = for {
      d0 <- decoded0
      d1 <- decoded1
    } yield combine(d0, d1)

    errors.fallback(both)(errors.fallback(decoded0)(decoded1))
  }

  implicit def combinedTraversableDecoder[F[_]: FlatMap, FF[T] <: TraversableOnce[T], A, S0, S1, I0, I1](
    implicit ev0: Lazy[DecoderT[F, S0, FF[A], I0]],
    ev1: Lazy[DecoderT[F, S1, FF[A], I1]],
    errors: ExtruderErrors[F],
    cbf: CanBuildFrom[FF[A], A, FF[A]]
  ): DecoderT[F, (S0, S1), FF[A], (I0, I1)] =
    DecoderT.make(
      combineOrFallback[F, FF[A], S0, S1, I0, I1](
        (d0, d1) => Parser.convertTraversable[A, FF](d0.toIterator ++ d1.toIterator)
      )
    )

  implicit def combinedNonEmptyChainDecoder[F[_]: FlatMap, A, S0, S1, I0, I1](
    implicit ev0: Lazy[DecoderT[F, S0, NonEmptyChain[A], I0]],
    ev1: Lazy[DecoderT[F, S1, NonEmptyChain[A], I1]],
    errors: ExtruderErrors[F]
  ): DecoderT[F, (S0, S1), NonEmptyChain[A], (I0, I1)] =
    DecoderT.make(combineOrFallback[F, NonEmptyChain[A], S0, S1, I0, I1]((d0, d1) => d0 ++ d1))

  implicit def combinedOptionalDecoder[F[_]: FlatMap, A, S0, S1, I0, I1](
    implicit ev0: Lazy[DecoderT[F, S0, Option[A], I0]],
    ev1: Lazy[DecoderT[F, S1, Option[A], I1]],
    errors: ExtruderErrors[F]
  ): DecoderT[F, (S0, S1), Option[A], (I0, I1)] =
    DecoderT.make(combineOrFallback[F, Option[A], S0, S1, I0, I1]({
      case (Some(d0), _) => Some(d0)
      case (None, Some(d1)) => Some(d1)
      case (None, None) => None
    }))
}
