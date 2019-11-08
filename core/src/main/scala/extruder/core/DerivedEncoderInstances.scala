package extruder.core

import cats.Applicative
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector}
import cats.kernel.Monoid
import shapeless.Lazy

trait DerivedEncoderInstances {
  implicit def nonEmptyListEncoder[F[_], A, S, O](
    implicit encoder: Lazy[Encoder[F, S, NonEmptyChain[A], O]]
  ): Encoder[F, S, NonEmptyList[A], O] =
    encoder.value.contramap(NonEmptyChain.fromNonEmptyList)

  implicit def nonEmptyVectorEncoder[F[_], A, S, O](
    implicit encoder: Lazy[Encoder[F, S, NonEmptyChain[A], O]]
  ): Encoder[F, S, NonEmptyVector[A], O] =
    encoder.value.contramap(NonEmptyChain.fromNonEmptyVector)

  implicit def nonEmptySetEncoder[F[_], A, S, O](
    implicit encoder: Lazy[Encoder[F, S, NonEmptyChain[A], O]]
  ): Encoder[F, S, NonEmptySet[A], O] =
    encoder.value.contramap(c => NonEmptyChain(c.head, c.tail.toSeq: _*))

  implicit def chainEncoder[F[_], A, S, O](
    implicit encoder: Lazy[Encoder[F, S, Vector[A], O]]
  ): Encoder[F, S, Chain[A], O] =
    encoder.value.contramap(_.toVector)

  implicit def nonEmptyChainEncoder[F[_], A, S, O](
    implicit encoder: Lazy[Encoder[F, S, Vector[A], O]]
  ): Encoder[F, S, NonEmptyChain[A], O] =
    encoder.value.contramap(_.toChain.toVector)

  implicit def optionalEncoder[F[_], T, S, O](
    implicit encoder: Lazy[Encoder[F, S, T, O]],
    F: Applicative[F],
    monoid: Monoid[O]
  ): Encoder[F, S, Option[T], O] =
    Encoder.make[F, S, Option[T], O] { (path, settings, value) =>
      value.fold[F[O]](F.pure(monoid.empty))(encoder.value.write(path, settings, _))
    }

  implicit def eitherEncoder[F[_], L, R, S, O](
    implicit leftEncoder: Lazy[Encoder[F, S, L, O]],
    rightEncoder: Lazy[Encoder[F, S, R, O]]
  ): Encoder[F, S, Either[L, R], O] = Encoder.make[F, S, Either[L, R], O] { (path, settings, value) =>
    value.fold(leftEncoder.value.write(path, settings, _), rightEncoder.value.write(path, settings, _))
  }
}
