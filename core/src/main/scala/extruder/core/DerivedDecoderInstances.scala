package extruder.core

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Functor, Monad}
import shapeless.{Lazy, Refute}

trait DerivedDecoderInstances {
  implicit def nonEmptyListDecoder[F[_]: Functor, T, S, D](
    implicit decoder: Lazy[Decoder[F, S, NonEmptyChain[T], D]]
  ): Decoder[F, S, NonEmptyList[T], D] = decoder.value.imap(_.toNonEmptyList)(NonEmptyChain.fromNonEmptyList)

  implicit def nonEmptyVectorDecoder[F[_]: Functor, T, S, D](
    implicit decoder: Lazy[Decoder[F, S, NonEmptyChain[T], D]]
  ): Decoder[F, S, NonEmptyVector[T], D] = decoder.value.imap(_.toNonEmptyVector)(NonEmptyChain.fromNonEmptyVector)

  implicit def chainDecoder[F[_]: Functor, T, S, D](
    implicit decoder: Lazy[Decoder[F, S, List[T], D]]
  ): Decoder[F, S, Chain[T], D] = decoder.value.imap(Chain.fromSeq)(_.toList)

  implicit def nonEmptyChainDecoder[F[_], T, S <: Settings, D](
    implicit decoder: Lazy[Decoder[F, S, Vector[T], D]],
    F: Monad[F],
    error: ExtruderErrors[F]
  ): Decoder[F, S, NonEmptyChain[T], D] = Decoder.make[F, S, NonEmptyChain[T], D] { (path, settings, default, data) =>
    val decoded = decoder.value.read(path, settings, default.map(_.toChain.toVector), data)

    decoded
      .map(NonEmptyChain.fromSeq)
      .flatMap[NonEmptyChain[T]](
        _.fold[F[NonEmptyChain[T]]](
          error.validationFailure(
            s"Collection at '${settings.pathElementListToString(path)}' must contain data, but is empty, and no default available"
          )
        )(F.pure)
      )
  }

  implicit def optionalDecoder[F[_], T, S, D](
    implicit decoder: Lazy[Decoder[F, S, T, D]],
    F: Monad[F],
    error: ExtruderErrors[F],
    hasValue: HasValue[F, S, D],
    refute: Refute[MultiParser[F, T]]
  ): Decoder[F, S, Option[T], D] = Decoder.make[F, S, Option[T], D] { (path, settings, default, data) =>
    lazy val decoded = decoder.value.read(path, settings, None, data).map(Option(_))
    lazy val lookedUp = hasValue(path, settings, data)

    error.fallback[Option[T]](decoded) {
      lookedUp.flatMap { lu =>
        if (lu) decoded
        else F.pure(default.flatten)
      }
    }
  }

  implicit def eitherDecoder[F[_], L, R, S, D](
    implicit leftDecoder: Lazy[Decoder[F, S, L, D]],
    F: Monad[F],
    hasValue: HasValue[F, S, D],
    rightDecoder: Lazy[Decoder[F, S, R, D]],
    error: ExtruderErrors[F]
  ): Decoder[F, S, Either[L, R], D] = Decoder.make[F, S, Either[L, R], D] { (path, settings, default, data) =>
    lazy val right: F[Either[L, R]] = rightDecoder.value.read(path, settings, None, data).map(Right(_))
    lazy val lookedUp: F[Boolean] = hasValue(path, settings, data)

    lazy val left: F[Either[L, R]] = leftDecoder.value.read(path, settings, None, data).map(Left(_))

    def maybeDefault: F[Option[Either[L, R]]] = lookedUp.map { lu =>
      default.flatMap { d =>
        if (lu) None
        else Some(d)
      }
    }

    def withDefault(fa: F[Either[L, R]]): F[Either[L, R]] =
      maybeDefault.flatMap(_.fold(fa)(d => error.fallback(fa)(F.pure(d))))

    withDefault(error.fallback(right)(left))
  }
}
