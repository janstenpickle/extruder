package extruder.core

import cats.instances.list._
import cats.syntax.functor._
import cats.kernel.Monoid
import cats.{Monad, Traverse}
import shapeless.{<:!<, LowPriority, Refute}

trait ShowEncoderTInstances {
  implicit def showEncoder[F[_], A, S, O](
    implicit shows: Show[A],
    writer: StringWriter[F, S, O],
    refute: Refute[MultiShow[A]],
    neOpt: A <:!< Option[_],
    lp: LowPriority
  ): EncoderT[F, S, A, O] =
    EncoderT.make { (path, settings, in) =>
      writer.write(path, settings, shows.show(in))
    }

  implicit def multiShowEncoder[F[_], A, S, O](
    implicit shows: MultiShow[A],
    F: Monad[F],
    writer: StringWriter[F, S, O],
    monoid: Monoid[O]
  ): EncoderT[F, S, A, O] =
    EncoderT.make { (path, settings, value) =>
      Traverse[List]
        .traverse(shows.show(value).toList) { case (p, v) => writer.write(path ++ p, settings, v) }
        .map(monoid.combineAll)
    }
}
