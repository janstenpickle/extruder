package extruder.core

import cats.{Monad, Monoid}
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import extruder.data.PathElement
import shapeless.{LowPriority, Refute}

trait MapEncoderInstances {
  implicit def stringMapEncoder[F[_], K, V, S, O](
    implicit encoder: Encoder[F, S, Map[String, String], O],
    keyShow: Show[K],
    valueShow: Show[V],
    lp: LowPriority
  ): Encoder[F, S, Map[K, V], O] = encoder.contramap(_.map { case (k, v) => (keyShow.show(k), valueShow.show(v)) })

  implicit def mapEncoder[F[_]: Monad, K, V, S, O](
    implicit keyShow: Show[K],
    valueEncoder: Encoder[F, S, V, O],
    monoid: Monoid[O],
    refute: Refute[MultiShow[V]]
  ): Encoder[F, S, Map[K, V], O] = Encoder.make[F, S, Map[K, V], O] { (path, settings, value) =>
    value.toList
      .traverse { case (k, v) => valueEncoder.write(path :+ PathElement.Standard(keyShow.show(k)), settings, v) }
      .map(monoid.combineAll)
  }
}
