package extruder.core

import cats.kernel.Monoid
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{FlatMap, Monad}

/**
  * Loads input data `I` with some side effect represented by functor `F`.
  *
  * @tparam F functor in which to wrap the result
  * @tparam I input data type
  */
trait LoadInput[F[_], I] {
  def load: F[I]
}

object LoadInput extends LowPriorityLoadInput {
  def apply[F[_], I](implicit loadInput: LoadInput[F, I]): LoadInput[F, I] = loadInput

  implicit def combinedLoadWithFallback[F[_]: Monad, I0, I1](
    implicit ev0: LoadInput[F, I0],
    ev1: LoadInput[F, I1],
    monoid0: Monoid[I0],
    monoid1: Monoid[I1],
    error: ExtruderErrors[F]
  ): LoadInput[F, (I0, I1)] =
    new LoadInput[F, (I0, I1)] {
      override def load: F[(I0, I1)] =
        for {
          i0 <- error.fallback(ev0.load)(monoid0.empty.pure[F])
          i1 <- error.fallback(ev1.load)(monoid1.empty.pure[F])
        } yield (i0, i1)
    }

}

trait LowPriorityLoadInput {
  implicit def combinedLoad[F[_]: FlatMap, I0, I1](
    implicit ev0: LoadInput[F, I0],
    ev1: LoadInput[F, I1]
  ): LoadInput[F, (I0, I1)] =
    new LoadInput[F, (I0, I1)] {
      override def load: F[(I0, I1)] =
        for {
          i0 <- ev0.load
          i1 <- ev1.load
        } yield (i0, i1)
    }
}
