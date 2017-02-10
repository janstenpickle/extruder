package extruder.fetch
import cats.data.NonEmptyList
import cats.syntax.show._
import cats.{Eval, Show}
import extruder.core.{BaseResolversSpec, Resolvers}
import fetch.{DataSource, Query}

import scala.concurrent.ExecutionContext.Implicits.global

class FetchResolversSpec extends BaseResolversSpec[String] {
  import BaseResolversSpec._
  import FetchResolversSpec._

  override implicit def showList: Show[String] = Show.show(identity)

  override def failingResolvers: Resolvers = resolvers(Map.empty)

  override def successfulResolvers[T: Show](value: T): Resolvers = resolvers(Map(testKey -> value.show))

  override def makeList[T](list: List[T]): String = list.mkString(",")
}

object FetchResolversSpec {
  def ds(data: Map[String, String]): DataSource[String, String] = new DataSource[String, String] {
    override def fetchOne(id: String): Query[Option[String]] = Query.eval(Eval.now(data.get(id)))

    override def fetchMany(ids: NonEmptyList[String]): Query[Map[String, String]] = Query.eval(Eval.now(
      ids.foldLeft(Map.empty[String, String])((acc, id) =>
        data.get(id).fold(acc)(v => acc + (id -> v))
      )
    ))
  }

  def resolvers(data: Map[String, String]): FetchResolvers[String] =
    FetchResolvers[String](ds(data), _.mkString("."))
}
