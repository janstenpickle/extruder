package extruder.core

import cats.Applicative
import cats.data.NonEmptyList
import cats.kernel.Monoid
import shapeless.Lazy

trait DerivedEncoderTInstances {
  implicit def nonEmptyListEncoder[F[_], A, S, O](
    implicit encoder: Lazy[EncoderT[F, S, List[A], O]]
  ): EncoderT[F, S, NonEmptyList[A], O] =
    encoder.value.contramap(_.toList)

  implicit def optionalEncoder[F[_], T, S, O](
    implicit encoder: Lazy[EncoderT[F, S, T, O]],
    F: Applicative[F],
    monoid: Monoid[O]
  ): EncoderT[F, S, Option[T], O] =
    EncoderT.make[F, S, Option[T], O] { (path, settings, value) =>
      value.fold[F[O]](F.pure(monoid.empty))(encoder.value.write(path, settings, _))
    }
}
