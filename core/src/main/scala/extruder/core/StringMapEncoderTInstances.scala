package extruder.core

import cats.instances.list._
import cats.syntax.functor._
import cats.kernel.Monoid
import cats.{Monad, Traverse}
import shapeless.Refute

trait StringMapEncoderTInstances {
  implicit def mapEncoder[F[_], K, V, S, D](
    implicit F: Monad[F],
    keyShow: Show[K],
    valueEncoder: EncoderT[F, S, V, D],
    monoid: Monoid[D],
    refute: Refute[MultiShow[V]]
  ): EncoderT[F, S, Map[K, V], D] = EncoderT.make[F, S, Map[K, V], D] { (path, settings, value) =>
    Traverse[List]
      .traverse(value.toList) { case (k, v) => valueEncoder.write(path :+ keyShow.show(k), settings, v) }
      .map(monoid.combineAll)
  }
}
