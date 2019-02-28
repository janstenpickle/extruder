package extruder.core

import extruder.data.PathElement
import extruder.instances.EncoderInstances

/**
  * Encode value `A` into data source `O`, wrapping result in functor `F`
  *
  * @tparam F functor to wrap output
  * @tparam S settings for encoding
  * @tparam A value to encode
  * @tparam O output data
  */
trait Encoder[F[_], S, A, O] {
  def write(path: List[PathElement], settings: S, in: A): F[O]
  def contramap[B](f: B => A): Encoder[F, S, B, O] = Encoder.make[F, S, B, O] { (path, settings, in) =>
    write(path, settings, f(in))
  }
}

object Encoder
    extends EncoderInstances
    with ShowEncoderInstances
    with MapEncoderInstances
    with DerivedEncoderInstances
    with GenericEncoderInstances
    with CombinedEncoderInstances {
  def make[F[_], S, A, O](f: (List[PathElement], S, A) => F[O]): Encoder[F, S, A, O] = new Encoder[F, S, A, O] {
    override def write(path: List[PathElement], settings: S, in: A): F[O] = f(path, settings, in)
  }

  def apply[F[_], S, A, O](implicit encoderT: Encoder[F, S, A, O]): Encoder[F, S, A, O] = encoderT
}
