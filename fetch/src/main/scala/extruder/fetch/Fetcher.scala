package extruder.fetch

import cats.syntax.either._
import cats.syntax.validated._
import extruder.core.{ConfigValidation, ResolversBase, ValidationFailure}
import fetch._
import fetch.implicits._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

trait Fetcher[I] { this: ResolversBase =>
  implicit def ec: ExecutionContext

  def timeout: Duration

  def pathToIdentity(path: Seq[String]): I

  def fetch[T](path: Seq[String])(implicit ds: DataSource[I, T]): ConfigValidation[Option[T]] =
    Either.catchNonFatal[T](Await.result(Fetch.run[Future](Fetch[I, T](pathToIdentity(path))), timeout)).
      fold[ConfigValidation[Option[T]]](
        {
          case _: NotFound => None.validNel
          case ex: Any => ValidationFailure(
            s"Failed to resolve value for '${pathToString(path)}': '${ex.getMessage}'", Some(ex)
          )
        },
        Some(_).validNel
      )
}
