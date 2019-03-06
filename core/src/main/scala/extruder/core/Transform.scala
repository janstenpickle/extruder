package extruder.core

import cats.data.{Ior, Kleisli}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, FlatMap, Functor}
import extruder.data.PathElement
import extruder.instances.TransformInstances
import shapeless.LowPriority

/**
  * Transforms encoded data or data to be decoded. Very similar to a Kleisli composition, can even be constructed
  * from such a composition.
  *
  * Takes input data `I` and transforms it into output data `O`, wrapping the result in functor `F`
  * using a provided namespace and settings.
  *
  * @tparam F functor to wrap the result in
  * @tparam S settings used when transforming
  * @tparam I input data
  * @tparam O output data
  */
trait Transform[F[_], S, I, O] { outer =>
  def run(namespace: List[PathElement], settings: S, input: I): F[O]
  def map[O0](f: O => O0)(implicit F: Functor[F]): Transform[F, S, I, O0] = new Transform[F, S, I, O0] {
    override def run(namespace: List[PathElement], settings: S, input: I): F[O0] =
      outer.run(namespace, settings, input).map(f)
  }

  def flatMapResult[O0](f: O => F[O0])(implicit F: FlatMap[F]): Transform[F, S, I, O0] = new Transform[F, S, I, O0] {
    override def run(namespace: List[PathElement], settings: S, input: I): F[O0] =
      outer.run(namespace, settings, input).flatMap(f)
  }
}

object Transform extends LowPriorityTransform with TransformInstances {
  def apply[F[_], S, I, O](implicit transform: Transform[F, S, I, O]): Transform[F, S, I, O] = transform

  def fromKleisli[F[_], S, I, O](k: Kleisli[F, (List[PathElement], S, I), O]): Transform[F, S, I, O] =
    new Transform[F, S, I, O] {
      override def run(namespace: List[PathElement], settings: S, input: I): F[O] =
        k.run((namespace, settings, input))
    }

  def by[F[_]: Functor, S, I, O, O0](f: O => O0)(implicit transform: Transform[F, S, I, O]): Transform[F, S, I, O0] =
    transform.map(f)

  def byF[F[_]: FlatMap, S, I, O, O0](
    f: O => F[O0]
  )(implicit transform: Transform[F, S, I, O]): Transform[F, S, I, O0] =
    transform.flatMapResult(f)

  def inputBy[F[_]: Functor, S, I, O](f: I => O)(implicit transform: Transform[F, S, I, I]): Transform[F, S, I, O] =
    transform.map(f)

  def inputByF[F[_]: FlatMap, S, I, O](f: I => F[O])(implicit transform: Transform[F, S, I, I]): Transform[F, S, I, O] =
    transform.flatMapResult(f)

  implicit def noOp[F[_]: Applicative, I, S]: Transform[F, S, I, I] = new Transform[F, S, I, I] {
    override def run(namespace: List[PathElement], settings: S, input: I): F[I] = input.pure[F]
  }

  implicit def tupleCombinedTransform[F[_]: FlatMap, S0, S1, I0, I1, O0, O1](
    implicit ev0: Transform[F, S0, I0, O0],
    ev1: Transform[F, S1, I1, O1],
    lp: LowPriority
  ): Transform[F, (S0, S1), (I0, I1), (O0, O1)] = new Transform[F, (S0, S1), (I0, I1), (O0, O1)] {
    override def run(namespace: List[PathElement], settings: (S0, S1), input: (I0, I1)): F[(O0, O1)] =
      for {
        o0 <- ev0.run(namespace, settings._1, input._1)
        o1 <- ev1.run(namespace, settings._2, input._2)
      } yield (o0, o1)
  }

}

trait LowPriorityTransform {
  implicit def iorCombinedTransform[F[_]: FlatMap, S0, S1, I0, I1, O0, O1](
    implicit ev0: Transform[F, S0, I0, O0],
    ev1: Transform[F, S1, I1, O1],
    error: ExtruderErrors[F]
  ): Transform[F, (S0, S1), Ior[I0, I1], Ior[O0, O1]] = new Transform[F, (S0, S1), Ior[I0, I1], Ior[O0, O1]] {
    override def run(namespace: List[PathElement], settings: (S0, S1), input: Ior[I0, I1]): F[Ior[O0, O1]] = {
      input match {
        case Ior.Left(i0) => ev0.run(namespace, settings._1, i0).map(Ior.Left(_))
        case Ior.Right(i1) => ev1.run(namespace, settings._2, i1).map(Ior.Right(_))
        case Ior.Both(i0, i1) =>
          lazy val both: F[Ior[O0, O1]] = for {
            o0 <- ev0.run(namespace, settings._1, i0)
            o1 <- ev1.run(namespace, settings._2, i1)
          } yield Ior.Both(o0, o1)

          lazy val either: F[Ior[O0, O1]] = error.fallback[Ior[O0, O1]](
            ev1.run(namespace, settings._2, i1).map(Ior.Right(_))
          )(ev0.run(namespace, settings._1, i0).map(Ior.Left(_)))

          error.fallback(both)(either)
      }
    }
  }
}
