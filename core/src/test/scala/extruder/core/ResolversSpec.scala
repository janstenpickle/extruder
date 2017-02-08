package extruder.core

import cats.Show
import cats.syntax.validated._
import cats.syntax.show._

class ResolversSpec extends BaseResolversSpec[String] {

  override val failingResolvers: Resolvers = new Resolvers {
    override def pathToString(path: Seq[String]): String = path.mkString("").toLowerCase
    override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = None.validNel
  }

  override def successfulResolvers[T : Show](value: T): Resolvers = new Resolvers {
    override def pathToString(path: Seq[String]): String = ""
    override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = Some(value.show).validNel
  }

  override def makeList[T](list: List[T]): String = list.mkString(",")


  override implicit val showList: Show[String] = Show.show(identity)
}
