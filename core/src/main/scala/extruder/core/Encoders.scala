package extruder.core

import cats.Monoid
import cats.syntax.validated._

trait Encoders[C, E[T] <: Encoder[T, C]] {
  protected def monoid: Monoid[C]

  protected def mkEncoder[T](f: (Seq[String], T) => ConfigValidation[C]): E[T]

  def apply[T](implicit encoder: E[T]): E[T] = encoder
}

trait Encode[I, O, E[T] <: Encoder[T, I]] {
  protected def finalizeConfig(inter: I): ConfigValidation[O]

  def encode[T](value: T)(implicit encoder: E[T]): ConfigValidation[O] = encode(Seq.empty, value)

  def encode[T](namespace: Seq[String], value: T)(implicit encoder: E[T]): ConfigValidation[O] =
    encoder.write(namespace, value).fold(_.invalid, finalizeConfig)
}

trait Encoder[I, O] {
  def write(path: Seq[String], in: I): ConfigValidation[O]
}
