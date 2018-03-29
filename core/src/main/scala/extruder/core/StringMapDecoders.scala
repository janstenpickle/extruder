package extruder.core

import cats.instances.list._
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Traverse}
import shapeless.Refute

trait StringMapDecoders { self: Decoders with DecodeTypes =>
  protected def prune[F[_]](path: List[String], data: DecodeData)(
    implicit F: Eff[F],
    hints: Hint
  ): F[Option[(List[String], DecodeData)]]

  implicit def mapDecoder[F[_], T](
    implicit F: Eff[F],
    decoder: Dec[F, T],
    hints: Hint,
    refute: Refute[MultiParser[T]]
  ): Dec[F, Map[String, T]] =
    mkDecoder(
      (path, default, data) =>
        for {
          pruned <- prune(path, data)
          parsed <- Traverse[Option]
            .traverse(pruned) { case (keys, p) => decodeMap[F, T](path)(keys, p) }
          result <- selectOption(path, parsed, default)
        } yield result
    )

  private def decodeMap[F[_]: Applicative, T](
    basePath: List[String]
  )(keys: List[String], data: DecodeData)(implicit decoder: Dec[F, T], hints: Hint): F[Map[String, T]] = {
    Traverse[List]
      .sequence[F, (String, T)](
        keys
          .map(k => decoder.read(basePath :+ k, None, data).map(k -> _))
      )
      .map(_.toMap)
  }
}
