package extruder.core

import cats.instances.list._
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, Traverse}
import extruder.data.Prune
import shapeless.Refute

trait StringMapDecoderTInstances extends OptionSelector {

  implicit def mapDecoder[F[_], K, V, S <: Settings, D](
    implicit F: Monad[F],
    error: ExtruderErrors[F],
    keyParser: Parser[K],
    decoder: DecoderT[F, S, V, D],
    prune: Prune[F, S, D],
    refute: Refute[MultiParser[F, V]]
  ): DecoderT[F, S, Map[K, V], D] =
    DecoderT.make[F, S, Map[K, V], D](
      (path, settings, default, data) =>
        for {
          pruned <- prune.prune(path, settings, data)
          parsed <- Traverse[Option]
            .traverse(pruned) { case (keys, p) => decodeMap[F, K, V, S, D](path, settings)(keys, p) }
          result <- selectOption[F, Map[K, V], S](path, settings, parsed, default)
        } yield result
    )

  private def decodeMap[F[_]: Monad, K, V, S, D](basePath: List[String], settings: S)(
    keys: List[String],
    data: D
  )(implicit error: ExtruderErrors[F], keyParser: Parser[K], valueDecoder: DecoderT[F, S, V, D]): F[Map[K, V]] = {
    Traverse[List]
      .sequence[F, (K, V)](
        keys
          .map(
            k =>
              valueDecoder
                .read(basePath :+ k, settings, None, data)
                .flatMap(v => error.fromEither(keyParser.parse(k)).map(_ -> v))
          )
      )
      .map(_.toMap)
  }
}
