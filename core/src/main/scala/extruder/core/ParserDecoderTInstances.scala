package extruder.core

import cats.data.{OptionT, ValidatedNel}
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, Traverse}
import extruder.data.StringReader

trait ParserDecoderTInstances extends Resolve {

  implicit def parserDecoder[F[_], T, S <: Settings, D](
    implicit parser: Parser[T],
    monad: Monad[F],
    error: ExtruderErrors[F],
    reader: StringReader[F, S, D]
  ): DecoderT[F, S, T, D] =
    DecoderT.make[F, S, T, D](
      (path, settings, default, data) =>
        resolveValue[F, S, T, D](formatParserError[F, T, S](parser, path, settings))
          .apply(path, settings, default, data)
    )

  implicit def optionalMultiParserDecoder[F[_], T, S <: Settings, D](
    implicit parser: MultiParser[F, T],
    monad: Monad[F],
    error: ExtruderErrors[F],
    reader: StringReader[F, S, D]
  ): DecoderT[F, S, Option[T], D] =
    DecoderT.make[F, S, Option[T], D]((path, settings, _, data) => multiParse[F, T, S, D](parser, path, settings, data))

  implicit def multiParserDecode[F[_], T, S <: Settings, D](
    implicit parser: MultiParser[F, T],
    monad: Monad[F],
    error: ExtruderErrors[F],
    reader: StringReader[F, S, D]
  ): DecoderT[F, S, T, D] =
    DecoderT.make[F, S, T, D](
      (path, settings, default, data) =>
        for {
          parsed <- multiParse[F, T, S, D](parser, path, settings, data)
          result <- selectOption[F, T, S](path, settings, parsed, default)
        } yield result
    )

  private def multiParse[F[_], T, S, D](parser: MultiParser[F, T], path: List[String], settings: S, data: D)(
    implicit F: Monad[F],
    reader: StringReader[F, S, D],
    error: ExtruderErrors[F]
  ): F[Option[T]] =
    parser
      .parse(p => OptionT(reader.lookup(path ++ p, settings, data)))
      .value
      .flatMap(
        v =>
          Traverse[Option]
            .sequence[ValidatedNel[String, ?], T](v)
            .fold[F[Option[T]]](
              errs =>
                errs.tail
                  .foldLeft(error.validationFailure[Option[T]](errs.head))(
                    (acc, v) =>
                      F.ap2[Option[T], Option[T], Option[T]](F.pure((l, _) => l))(acc, error.validationFailure(v))
                ),
              F.pure
          )
      )
}
