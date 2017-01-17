package extruder.core

import cats.Show
import cats.syntax.show._

class MapResolversSpec extends BaseResolversSpec[String] {
  import BaseResolversSpec._

  override def failingResolvers: Resolvers = MapResolvers(Map.empty)

  override def successfulResolvers[T : Show](value: T): Resolvers = MapResolvers(Map(testKey -> value.show))

  override def makeList[T](list: List[T]): String = list.mkString(",")

  override implicit val showList: Show[String] = Show.show(identity)
}
