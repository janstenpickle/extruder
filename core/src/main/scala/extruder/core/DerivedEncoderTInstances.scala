package extruder.core

import cats.Applicative
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.kernel.Monoid
import shapeless.Lazy

trait DerivedEncoderTInstances {
  implicit def nonEmptyListEncoder[F[_], A, S, O](
    implicit encoder: Lazy[EncoderT[F, S, NonEmptyChain[A], O]]
  ): EncoderT[F, S, NonEmptyList[A], O] =
    encoder.value.contramap(NonEmptyChain.fromNonEmptyList)

  implicit def nonEmptyVectorEncoder[F[_], A, S, O](
    implicit encoder: Lazy[EncoderT[F, S, NonEmptyChain[A], O]]
  ): EncoderT[F, S, NonEmptyVector[A], O] =
    encoder.value.contramap(NonEmptyChain.fromNonEmptyVector)

  implicit def chainEncoder[F[_], A, S, O](
    implicit encoder: Lazy[EncoderT[F, S, Vector[A], O]]
  ): EncoderT[F, S, Chain[A], O] =
    encoder.value.contramap(_.toVector)

  implicit def nonEmptyChainEncoder[F[_], A, S, O](
    implicit encoder: Lazy[EncoderT[F, S, Vector[A], O]]
  ): EncoderT[F, S, NonEmptyChain[A], O] =
    encoder.value.contramap(_.toChain.toVector)

  implicit def optionalEncoder[F[_], T, S, O](
    implicit encoder: Lazy[EncoderT[F, S, T, O]],
    F: Applicative[F],
    monoid: Monoid[O]
  ): EncoderT[F, S, Option[T], O] =
    EncoderT.make[F, S, Option[T], O] { (path, settings, value) =>
      value.fold[F[O]](F.pure(monoid.empty))(encoder.value.write(path, settings, _))
    }

  implicit def eitherEncoder[F[_], L, R, S, O](
    implicit leftEncoder: Lazy[EncoderT[F, S, L, O]],
    rightEncoder: Lazy[EncoderT[F, S, R, O]]
  ): EncoderT[F, S, Either[L, R], O] = EncoderT.make[F, S, Either[L, R], O] { (path, settings, value) =>
    value.fold(leftEncoder.value.write(path, settings, _), rightEncoder.value.write(path, settings, _))
  }
}
