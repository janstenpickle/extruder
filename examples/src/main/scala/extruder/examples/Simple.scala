package extruder.examples

import extruder.core.MapResolvers
import extruder.resolution._

import scala.concurrent.duration.{Duration, FiniteDuration}

case class CC(a: String = "test", b: String = "test2", c: Int, d: Option[CC2], e: CC3, f: Set[Int], dur: Duration, finDur: FiniteDuration)
case class CC2(x: String = "test4", y: Option[Int] = Some(232), z: CC3)
case class CC3(a: Option[String])
case class CC4(a: Option[CC3])

sealed trait Sealed
case object ObjImpl extends Sealed
case class CCImpl(a: String) extends Sealed

object Simple extends App {
  val ccResolvers = MapResolvers(Map(
    "cc.c" -> "2000",
    "cc.e.cc3.a" -> "test3",
    "cc.d.cc2.z.cc3.a" -> "testing",
    "cc3.a" -> "hello",
    "cc.f" -> "2, 3",
    "cc.dur" -> "Inf",
    "cc.findur" -> "22 days"
  ))

  println(resolve[CC, MapResolvers](ccResolvers))

  val sealedCCResolvers = MapResolvers(Map(
    "type" -> "CCImpl",
    "ccimpl.a" -> "testing"
  ))

  println(resolve[Sealed, MapResolvers](sealedCCResolvers))

 val sealedObjResolvers = MapResolvers(Map("type" -> "ObjImpl"))

  println(resolve[Sealed, MapResolvers](sealedObjResolvers))
}
