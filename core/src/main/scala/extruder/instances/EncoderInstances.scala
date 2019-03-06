package extruder.instances

import cats.Contravariant
import extruder.core.Encoder

trait EncoderInstances {
  implicit def extruderStdInstancesForEncoderT[F[_], S, O]: Contravariant[Encoder[F, S, ?, O]] =
    new Contravariant[Encoder[F, S, ?, O]] {
      override def contramap[A, B](fa: Encoder[F, S, A, O])(f: B => A): Encoder[F, S, B, O] = fa.contramap(f)
    }
}
