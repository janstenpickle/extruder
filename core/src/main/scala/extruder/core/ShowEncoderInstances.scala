package extruder.core

import cats.instances.list._
import cats.syntax.functor._
import cats.kernel.Monoid
import cats.{Monad, Traverse}
import extruder.data.PathElement
import shapeless.{<:!<, LowPriority, Refute}

trait ShowEncoderInstances {
  implicit def showEncoder[F[_], A, S, O](
    implicit shows: Show[A],
    writer: StringWriter[F, S, O],
    refute: Refute[MultiShow[A]],
    neOpt: A <:!< Option[_],
    lp: LowPriority
  ): Encoder[F, S, A, O] =
    Encoder.make { (path, settings, in) =>
      writer.write(path, settings, shows.show(in))
    }

  implicit def multiShowEncoder[F[_], A, S, O](
    implicit shows: MultiShow[A],
    F: Monad[F],
    writer: StringWriter[F, S, O],
    monoid: Monoid[O]
  ): Encoder[F, S, A, O] =
    Encoder.make { (path, settings, value) =>
      Traverse[List]
        .traverse(shows.show(value).toList) {
          case (p, v) => writer.write(path ++ p.map(PathElement.Standard), settings, v)
        }
        .map(monoid.combineAll)
    }
}
