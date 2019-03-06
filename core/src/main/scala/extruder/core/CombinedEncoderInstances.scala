package extruder.core

import cats.FlatMap
import cats.data.Ior
import cats.syntax.flatMap._
import cats.syntax.functor._
import extruder.data.PathElement
import shapeless.LowPriority

trait CombinedEncoderInstances {
  implicit def combinedEncoderWithFallback[F[_]: FlatMap, A, S0, S1, O0, O1](
    implicit ev0: Encoder[F, S0, A, O0],
    ev1: Encoder[F, S1, A, O1],
    error: ExtruderErrors[F]
  ): Encoder[F, (S0, S1), A, Ior[O0, O1]] = new Encoder[F, (S0, S1), A, Ior[O0, O1]] {
    override def write(path: List[PathElement], settings: (S0, S1), in: A): F[Ior[O0, O1]] = {
      lazy val both: F[Ior[O0, O1]] = for {
        o0 <- ev0.write(path, settings._1, in)
        o1 <- ev1.write(path, settings._2, in)
      } yield Ior.Both(o0, o1)

      lazy val either: F[Ior[O0, O1]] =
        error.fallback[Ior[O0, O1]](ev0.write(path, settings._1, in).map(Ior.Left(_)))(
          ev1.write(path, settings._2, in).map(Ior.Right(_))
        )

      error.fallback(both)(either)
    }
  }

  implicit def combinedEncoder[F[_]: FlatMap, A, S0, S1, O0, O1](
    implicit ev0: Encoder[F, S0, A, O0],
    ev1: Encoder[F, S1, A, O1],
    lp: LowPriority
  ): Encoder[F, (S0, S1), A, Ior[O0, O1]] = new Encoder[F, (S0, S1), A, Ior[O0, O1]] {
    override def write(path: List[PathElement], settings: (S0, S1), in: A): F[Ior[O0, O1]] =
      for {
        o0 <- ev0.write(path, settings._1, in)
        o1 <- ev1.write(path, settings._2, in)
      } yield Ior.Both(o0, o1)
  }
}
