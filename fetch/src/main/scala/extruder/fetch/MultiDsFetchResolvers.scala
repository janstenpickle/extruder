package extruder.fetch

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import extruder.core.{Resolver, ResolversBase, ValidationFailure}
import fetch._

import scala.collection.generic.CanBuildFrom

trait MultiDsFetchResolvers[I] extends ResolversBase with Fetcher[I] {
  def pathToIdentity(path: Seq[String]): I

  implicit def resolve[T](implicit ds: DataSource[I, T]): Resolver[T] =
    Resolver[T]((path: Seq[String], default: Option[T]) =>
      (fetch[T](path), default) match {
        case (Valid(None), None) => errorMsg[T](path)
        case (Valid(None), Some(v)) => v.validNel[ValidationFailure]
        case (Valid(Some(v)), _) => v.validNel[ValidationFailure]
        case (err @ Invalid(_), _) => err
      }
    )

  implicit def optional[T](implicit resolver: Resolver[T], ds: DataSource[I, T]): Resolver[Option[T]] =
    Resolver((path, default) =>
      (fetch[T](path), default) match {
        case (Valid(None), None) => None.validNel
        case (Valid(None), Some(d)) => d.validNel
        case (Valid(Some(_)), _) => resolver.read(path, None).map(Some(_))
        case (err @ Invalid(_), _) => err
      }
    )

  implicit def traversableOnce[T, F[T] <: TraversableOnce[T]](implicit ds: DataSource[I, List[T]],
                                                              cbf: CanBuildFrom[F[T], T, F[T]]): Resolver[F[T]] =
    Resolver[F[T]]((path: Seq[String], default: Option[F[T]]) =>
      (fetch[List[T]](path), default) match {
        case (Valid(None), None) => errorMsg[F[T]](path)
        case (Valid(None), Some(v)) => v.validNel[ValidationFailure]
        case (Valid(Some(v)), _) => v.foldLeft(cbf())(_ += _).result().validNel[ValidationFailure]
        case (err @ Invalid(_), _) => err
      }
    )
}
