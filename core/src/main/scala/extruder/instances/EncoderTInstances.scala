package extruder.instances

import cats.Contravariant
import extruder.core.EncoderT

trait EncoderTInstances {
  implicit def extruderStdInstancesForEncoderT[F[_], S, O]: Contravariant[EncoderT[F, S, ?, O]] =
    new Contravariant[EncoderT[F, S, ?, O]] {
      override def contramap[A, B](fa: EncoderT[F, S, A, O])(f: B => A): EncoderT[F, S, B, O] = fa.contramap(f)
    }
}
