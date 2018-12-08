package extruder.data

import cats.Functor
import cats.syntax.functor._

trait HasValue[F[_], S, I] {
  def apply(path: List[String], settings: S, data: I): F[Boolean] = hasValue(path, settings, data)
  def hasValue(path: List[String], settings: S, data: I): F[Boolean]
}

object HasValue {
  def apply[F[_], S, I](implicit hasValue: HasValue[F, S, I]): HasValue[F, S, I] = hasValue

  implicit def fromStringReader[F[_]: Functor, S, I](implicit stringReader: StringReader[F, S, I]): HasValue[F, S, I] =
    new HasValue[F, S, I] {
      override def hasValue(path: List[String], settings: S, data: I): F[Boolean] =
        stringReader.lookup(path, settings, data).map(_.isDefined)
    }
}
