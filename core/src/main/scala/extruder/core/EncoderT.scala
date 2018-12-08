package extruder.core
import extruder.instances.EncoderTInstances

trait EncoderT[F[_], S, A, O] {
  def write(path: List[String], settings: S, in: A): F[O]
  def contramap[B](f: B => A): EncoderT[F, S, B, O] = EncoderT.make[F, S, B, O] { (path, settings, in) =>
    write(path, settings, f(in))
  }
}

object EncoderT
    extends EncoderTInstances
    with ShowEncoderTInstances
    with StringMapEncoderTInstances
    with DerivedEncoderTInstances
    with GenericEncoderTInstances
    with CombinedEncoderTInstances {
  def make[F[_], S, A, O](f: (List[String], S, A) => F[O]): EncoderT[F, S, A, O] = new EncoderT[F, S, A, O] {
    override def write(path: List[String], settings: S, in: A): F[O] = f(path, settings, in)
  }

  def apply[F[_], S, A, O](implicit encoderT: EncoderT[F, S, A, O]): EncoderT[F, S, A, O] = encoderT
}
