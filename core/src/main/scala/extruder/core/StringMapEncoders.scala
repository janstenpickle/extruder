package extruder.core

import cats.Traverse
import cats.instances.list._
import cats.syntax.functor._
import shapeless.Refute

trait StringMapEncoders { self: Encoders with EncodeTypes =>
  implicit def mapEncoder[F[_], T](
    implicit F: Eff[F],
    encoder: Enc[F, T],
    refute: Refute[MultiShow[T]]
  ): Enc[F, Map[String, T]] = mkEncoder { (path, settings, value) =>
    Traverse[List]
      .traverse(value.toList) { case (k, v) => encoder.write(path :+ k, settings, v) }
      .map(monoid.combineAll)
  }
}
