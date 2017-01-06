package shapelessconfig.examples

import shapelessconfig.core.SystemPropertiesResolvers
import shapelessconfig.core.macros.TopLevelConfig

case class CC(a: String = "test", b: String = "test2", c: Int, d: CC2, e: CC3)

case class CC2(x: String = "test4", y: Option[Int] = Some(232), z: CC3)

case class CC3(a: Option[String] = Some("dsfds"))

object Examples extends App {
  println(TopLevelConfig.resolve[CC, SystemPropertiesResolvers]())
}
