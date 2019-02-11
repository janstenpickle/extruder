package extruder.core

import cats.Monad
import cats.instances.list._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import shapeless.{LowPriority, Refute}

trait MapDecoderTInstances extends OptionSelector {

  implicit def stringMapDecoder[F[_]: Monad, K, V, S, I](
    implicit mapDecoder: DecoderT[F, S, Map[String, String], I],
    keyParser: Parser[K],
    valueParser: Parser[V],
    keyShow: Show[K],
    valueShow: Show[V],
    error: ExtruderErrors[F],
    refute: Refute[MultiParser[F, V]],
    lp: LowPriority
  ): DecoderT[F, S, Map[K, V], I] =
    mapDecoder.imapResult(
      _.toList
        .traverse[F, (K, V)] {
          case (k, v) => error.fromEither(keyParser.parse(k)).map2(error.fromEither(valueParser.parse(v)))((_, _))
        }
        .map(_.toMap)
    )(_.map { case (k, v) => (keyShow.show(k), valueShow.show(v)) })

  implicit def mapDecoder[F[_]: Monad, K, V, S <: Settings, I](
    implicit
    error: ExtruderErrors[F],
    keyParser: Parser[K],
    decoder: DecoderT[F, S, V, I],
    prune: Prune[F, S, I],
    refute: Refute[MultiParser[F, V]]
  ): DecoderT[F, S, Map[K, V], I] =
    DecoderT.make[F, S, Map[K, V], I](
      (path, settings, default, data) =>
        for {
          pruned <- prune.prune(path, settings, data)
          parsed <- pruned.traverse { case (keys, p) => decodeMap[F, K, V, S, I](path, settings)(keys, p) }
          result <- selectOption[F, Map[K, V], S](path, settings, parsed, default)
        } yield result
    )

  private def decodeMap[F[_]: Monad, K, V, S, I](basePath: List[String], settings: S)(
    keys: List[String],
    data: I
  )(implicit error: ExtruderErrors[F], keyParser: Parser[K], valueDecoder: DecoderT[F, S, V, I]): F[Map[K, V]] = {
    keys
      .traverse[F, (K, V)](
        k =>
          valueDecoder
            .read(basePath :+ k, settings, None, data)
            .flatMap(v => error.fromEither(keyParser.parse(k)).map(_ -> v))
      )
      .map(_.toMap)
  }
}
