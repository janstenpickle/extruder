package extruder.core

import cats.syntax.validated._

class ResolversSpec extends BaseResolversSpec {

  override val failingResolvers: Resolvers = new Resolvers {
    override def pathToString(path: Seq[String]): String = path.mkString("").toLowerCase
    override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = None.validNel
  }

  override def successfulResolvers(value: Any): Resolvers = new Resolvers {
    override def pathToString(path: Seq[String]): String = ""
    override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = Some(value.toString).validNel
  }

  override def makeList[T](list: List[T]): Any = list.mkString(",")
}
