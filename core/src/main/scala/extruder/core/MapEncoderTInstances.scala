package extruder.core

import cats.{Monad, Monoid}
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import shapeless.{LowPriority, Refute}

trait MapEncoderTInstances {
  implicit def stringMapEncoder[F[_], K, V, S, O](
    implicit encoder: EncoderT[F, S, Map[String, String], O],
    keyShow: Show[K],
    valueShow: Show[V],
    lp: LowPriority
  ): EncoderT[F, S, Map[K, V], O] = encoder.contramap(_.map { case (k, v) => (keyShow.show(k), valueShow.show(v)) })

  implicit def mapEncoder[F[_]: Monad, K, V, S, O](
    implicit keyShow: Show[K],
    valueEncoder: EncoderT[F, S, V, O],
    monoid: Monoid[O],
    refute: Refute[MultiShow[V]]
  ): EncoderT[F, S, Map[K, V], O] = EncoderT.make[F, S, Map[K, V], O] { (path, settings, value) =>
    value.toList
      .traverse { case (k, v) => valueEncoder.write(path :+ keyShow.show(k), settings, v) }
      .map(monoid.combineAll)
  }
}
