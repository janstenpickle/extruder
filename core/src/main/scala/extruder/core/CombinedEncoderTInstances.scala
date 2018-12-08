package extruder.core

import cats.FlatMap
import cats.data.Ior
import cats.syntax.flatMap._
import cats.syntax.functor._
import shapeless.LowPriority

trait CombinedEncoderTInstances {
  implicit def combinedEncoder[F[_]: FlatMap, A, S0, S1, O0, O1](
    implicit ev0: EncoderT[F, S0, A, O0],
    ev1: EncoderT[F, S1, A, O1],
    error: ExtruderErrors[F],
    lp: LowPriority
  ): EncoderT[F, (S0, S1), A, Ior[O0, O1]] = new EncoderT[F, (S0, S1), A, Ior[O0, O1]] {
    override def write(path: List[String], settings: (S0, S1), in: A): F[Ior[O0, O1]] = {
      lazy val both: F[Ior[O0, O1]] = for {
        o0 <- ev0.write(path, settings._1, in)
        o1 <- ev1.write(path, settings._2, in)
      } yield Ior.Both(o0, o1)

      lazy val either: F[Ior[O0, O1]] =
        error.fallback[Ior[O0, O1]](ev1.write(path, settings._2, in).map(Ior.Right(_)))(
          ev0.write(path, settings._1, in).map(Ior.Left(_))
        )

      error.fallback(both)(either)
    }
  }
}
