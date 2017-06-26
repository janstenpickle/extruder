package extruder.core

import cats.effect.IO
import cats.syntax.all._
import cats.{Applicative, FlatMap}
import extruder.instances.{ConfigValidationInstances, EitherInstances, FutureInstances, IOInstances}

abstract class IOFlatMap[F[_]](implicit F: Applicative[F]) extends FlatMap[IOF[F, ?]] {
  override def tailRecM[A, B](a: A)(f: (A) => IOF[F, Either[A, B]]): IOF[F, B] =
    flatMap(f(a)){
      case Left(a1) => tailRecM(a1)(f)
      case Right(b) => IO.pure(F.pure(b))
    }

  override def map[A, B](fa: IOF[F, A])(f: (A) => B): IOF[F, B] =
    fa.map(_.map(f))
}

object IOFlatMap extends ConfigValidationInstances with FutureInstances with EitherInstances with IOInstances {
  def apply[F[_]](implicit iofFlatMap: IOFlatMap[F]): IOFlatMap[F] = iofFlatMap
}
