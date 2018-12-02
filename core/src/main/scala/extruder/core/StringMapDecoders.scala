package extruder.core

import cats.instances.list._
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Traverse}
import shapeless.Refute

trait StringMapDecoders { self: Decoders with DecodeTypes =>
  protected def prune[F[_]](path: List[String], settings: Sett, data: DecodeData)(
    implicit F: DecEff[F]
  ): F[Option[(List[String], DecodeData)]]

  implicit def mapDecoder[F[_], T](
    implicit F: DecEff[F],
    error: ExtruderErrors[F],
    decoder: DecT[F, T],
    refute: Refute[MultiParser[F, T]]
  ): DecT[F, Map[String, T]] =
    mkDecoder(
      (path, settings, default, data) =>
        for {
          pruned <- prune(path, settings, data)
          parsed <- Traverse[Option]
            .traverse(pruned) { case (keys, p) => decodeMap[F, T](path, settings)(keys, p) }
          result <- selectOption(path, settings, parsed, default)
        } yield result
    )

  private def decodeMap[F[_]: Applicative, T](
    basePath: List[String],
    settings: Sett
  )(keys: List[String], data: DecodeData)(implicit decoder: DecT[F, T]): F[Map[String, T]] = {
    Traverse[List]
      .sequence[F, (String, T)](
        keys
          .map(k => decoder.read(basePath :+ k, settings, None, data).map(k -> _))
      )
      .map(_.toMap)
  }
}
