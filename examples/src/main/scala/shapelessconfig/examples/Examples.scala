package shapelessconfig.examples

import shapelessconfig.core.SystemPropertiesResolvers
import shapelessconfig.macros.resolution._

case class CC(a: String = "test", b: String = "test2", c: Int, d: Option[CC2], e: CC3)

case class CC2(x: String = "test4", y: Option[Int] = Some(232), z: CC3)

case class CC3(a: Option[String])

case class CC4(a: Option[CC3])

object Examples extends App {
  System.setProperty("cc.c", "2000")
  System.setProperty("cc.e.cc3.a", "test3")
  System.setProperty("cc.d.cc2.z.cc3.a", "shit")
  System.setProperty("cc3.a", "hello")

  println(resolve[CC](SystemPropertiesResolvers))
  println(resolve[CC4].singletons(SystemPropertiesResolvers))
}
