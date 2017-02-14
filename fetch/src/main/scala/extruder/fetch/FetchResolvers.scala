package extruder.fetch

import extruder.core.{ConfigValidation, Resolvers}
import fetch._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait FetchResolvers[I] extends Resolvers with Fetcher[I] {
  implicit def ds: DataSource[I, String]

  override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = fetch[String](path)
}

object FetchResolvers {
  def apply[I](dataSource: DataSource[I, String],
               identity: Seq[String] => I,
               to: Duration = 1.minute)
              (implicit executionContext: ExecutionContext): FetchResolvers[I] = new FetchResolvers[I] {
    override implicit val ec: ExecutionContext = executionContext
    override implicit val ds: DataSource[I, String] = dataSource

    override def pathToIdentity(path: Seq[String]): I = identity(path)
    override def pathToString(path: Seq[String]): String = pathToIdentity(path).toString

    override val timeout: Duration = to
  }
}
