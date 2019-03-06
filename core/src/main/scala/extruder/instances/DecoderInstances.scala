package extruder.instances

import cats.{Functor, Invariant}
import extruder.core.Decoder

trait DecoderInstances {
  implicit def extruderStdInstancesForDecoderT[F[_]: Functor, S, O]: Invariant[Decoder[F, S, ?, O]] =
    new Invariant[Decoder[F, S, ?, O]] {
      override def imap[A, B](fa: Decoder[F, S, A, O])(f: A => B)(g: B => A): Decoder[F, S, B, O] = fa.imap(f)(g)
    }
}
