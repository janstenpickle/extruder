package extruder.core

class SystemPropertiesResolversSpec extends BaseResolversSpec {
  import BaseResolversSpec._

  override def failingResolvers: Resolvers = {
    System.clearProperty(testKey)
    new SystemPropertiesResolvers
  }

  override def successfulResolvers(value: Any): Resolvers = {
    System.setProperty(testKey, value.toString)
    new SystemPropertiesResolvers
  }

  override def makeList[T](list: List[T]): Any = list.mkString(",")
}
