package extruder.core

import cats.Show
import cats.syntax.show._

class SystemPropertiesResolversSpec extends BaseResolversSpec[String] {
  import BaseResolversSpec._

  override def failingResolvers: Resolvers = {
    System.clearProperty(testKey)
    new SystemPropertiesResolvers
  }

  override def successfulResolvers[T : Show](value: T): Resolvers = {
    System.setProperty(testKey, value.show)
    new SystemPropertiesResolvers
  }

  override def makeList[T](list: List[T]): String = list.mkString(",")

  override implicit val showList: Show[String] = Show.show(identity)
}
