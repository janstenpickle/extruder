package extruder.examples

import java.net.URL

import cats.syntax.either._
import cats.syntax.validated._
import com.typesafe.config.ConfigFactory
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import extruder.core._
import extruder.typesafe.TypesafeConfig

import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, FiniteDuration}
import extruder.refined._

case class CC(a: String = "test", b: String = "test2", c: Int, d: Option[CC2], e: CC3, f: Set[Int], dur: Duration, finDur: FiniteDuration)
case class CC2(x: String = "test4", y: Option[Int] = Some(232), z: CC3)
case class CC3(a: Option[String])
case class CC4(a: Option[CC3])


case class Testing(s: Option[Long], d: Set[String], i: Int = 1, j: ObjImpl.type)

sealed trait Sealed
case object ObjImpl extends Sealed
case class CCImpl2(SSHOULDNTA: String, shouldntI: Int = 3) extends Sealed
case class CCImpl3(b: String, c: Option[CCImpl2]) extends Sealed

//case class CCImpl(a: String, i: Long = 76, u: URL, s: Set[Int], cc: Option[CC4]) extends Sealed

case class Testing2(a: Set[Int] = Set(1,2), df: Option[Long] = Some(32L), dsf: Option[List[String]] = Some(List("dfs", "sdfsdf")))
case class Testing3(b: String, c: Testing2)

case class Hello(s: Sealed)

trait Thing[T] {
  def t: T
}
case class ThingImpl(t: String) extends Thing[String]


object Simple extends App {
  val config = Map(
    "cc.c" -> "2000",
    "cc.a" -> "sdfsf",
    "cc.e.cc3.a" -> "test3",
    "cc.d.cc2.z.cc3.a" -> "testing",
    "cc3.a" -> "hello",
    "cc.f" -> "2, 3",
    "cc.dur" -> "Inf",
    "cc.findur" -> "22 days"
  )

//  println(MapConfig.decode[CC](config))
//
//  val sealedObjResolvers = Map("type" -> "ObjImpl")
//
//  println(MapConfig.decode[Sealed](sealedObjResolvers).map(MapConfig.encode[Sealed]))
//
//  println(MapConfig.encode[Sealed](ObjImpl))
//
//
//  println(MapConfig.decode[CC](config).map(MapConfig.encode[CC]))
//
//  println(MapConfig.decode[Sealed](sealedObjResolvers))
//
//
//  println(SystemPropertiesConfig.decode[CC])
//
//  println(TypesafeConfig.decode[CC](ConfigFactory.parseMap(config.asJava)))
//
//  println(MapConfig.decode[Int Refined Positive](Map("" -> "23")).map(MapConfig.encode[Int Refined Positive]))
//
//  println(EnvironmentConfig.decode[String](Seq("home")))


  println(MapConfig.parameters[CCImpl3](Seq.empty[String]))
 // println(EnvironmentConfig.parameters[Sealed])

}


