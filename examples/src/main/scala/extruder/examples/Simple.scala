package extruder.examples

import extruder.core.SystemPropertiesResolvers
import extruder.resolution._

import scala.util.Try

case class CC(a: String = "test", b: String = "test2", c: Int, d: Option[CC2], e: CC3, f: Set[Int])

case class CC2(x: String = "test4", y: Option[Int] = Some(232), z: CC3)

case class CC3(a: Option[String]) {
  val tryA: Try[String] = Try(a.get)
}

case class CC4(a: Option[CC3])

object Simple extends App {
  System.setProperty("cc.c", "2000")
  System.setProperty("cc.e.cc3.a", "test3")
  System.setProperty("cc.d.cc2.z.cc3.a", "shit")
  System.setProperty("cc3.a", "hello")
  System.setProperty("cc.f", "2, 3")

  println(resolve[CC](SystemPropertiesResolvers))
  println(resolve[CC4].singletons(SystemPropertiesResolvers))
}
