package extruder.core

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.data.NonEmptyList
import extruder.data.HasValue
import shapeless.{Lazy, Refute}

trait DerivedDecoderTInstances {
  implicit def nonEmptyListDecoder[F[_], T, S <: Settings, D](
    implicit decoder: Lazy[DecoderT[F, S, List[T], D]],
    F: Monad[F],
    error: ExtruderErrors[F]
  ): DecoderT[F, S, NonEmptyList[T], D] = DecoderT.make[F, S, NonEmptyList[T], D] { (path, settings, default, data) =>
    val decoded = decoder.value.read(path, settings, default.map(_.toList), data)
    decoded
      .map(NonEmptyList.fromList)
      .flatMap[NonEmptyList[T]](
        _.fold[F[NonEmptyList[T]]](
          error.validationFailure(
            s"List at '${settings.pathToString(path)}' must contain data, but is empty, and no default available"
          )
        )(F.pure)
      )
  }

  implicit def optionalDecoder[F[_], T, S <: Settings, D](
    implicit decoder: Lazy[DecoderT[F, S, T, D]],
    F: Monad[F],
    error: ExtruderErrors[F],
    hasValue: HasValue[F, S, D],
    refute: Refute[MultiParser[F, T]]
  ): DecoderT[F, S, Option[T], D] = DecoderT.make[F, S, Option[T], D] { (path, settings, default, data) =>
    val decoded = decoder.value.read(path, settings, None, data).map(Option(_))
    val lookedUp = hasValue(path, settings, data)

    error.fallback[Option[T]](decoded) {
      lookedUp.flatMap { lu =>
        if (lu) decoded
        else F.pure(default.flatten)
      }
    }
  }
}
