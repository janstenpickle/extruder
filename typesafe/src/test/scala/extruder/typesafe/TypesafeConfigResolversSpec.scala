package extruder.typesafe

import com.typesafe.config.ConfigFactory
import extruder.core.{BaseResolversSpec, Resolvers}

import scala.collection.JavaConverters._

class TypesafeConfigResolversSpec extends BaseResolversSpec {
  import BaseResolversSpec._

  override val failingResolvers: Resolvers =
    TypesafeConfigResolvers(ConfigFactory.parseMap(Map.empty[String, Any].asJava))

  override def successfulResolvers(value: Any): Resolvers =
    TypesafeConfigResolvers(ConfigFactory.parseMap(Map(testKey -> value).asJava))

  override def makeList[T](list: List[T]): Any = list.asJava
}
