package extruder.instances

import cats.{Functor, Invariant}
import extruder.core.DecoderT

trait DecoderTInstances {
  implicit def extruderStdInstancesForDecoderT[F[_]: Functor, S, O]: Invariant[DecoderT[F, S, ?, O]] =
    new Invariant[DecoderT[F, S, ?, O]] {
      override def imap[A, B](fa: DecoderT[F, S, A, O])(f: A => B)(g: B => A): DecoderT[F, S, B, O] = fa.imap(f)(g)
    }
}
