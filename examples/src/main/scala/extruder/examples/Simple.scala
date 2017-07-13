package extruder.examples

import java.net.URL

import extruder.core.MapSource._
import extruder.core.{EitherErrors, EitherThrowable, ValidationErrors}
import extruder.monix._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}

case class CC(
  a: String = "test",
  b: String = "test2",
  c: Int,
  d: CC2,
  e: CC3,
  f: Set[Int],
  dur: Duration,
  finDur: FiniteDuration
)
case class CC2(x: String = "test4", y: Option[Int] = Some(232), z: CC3, dfs: Long)
case class CC3(a: Option[String])
case class CC4(a: Option[CC3])

sealed trait Sealed
case object ObjImpl extends Sealed
case class CCImpl(a: String, i: Long = 76, u: URL, s: Set[Int], cc: Option[CC4]) extends Sealed

object Simple extends App {

  val config = Map(
    "cc.c" -> "2000",
    "cc.a" -> "sdfsf",
    "cc.e.cc3.a" -> "test3",
    "cc.d.cc2.z.cc3.a" -> "testing",
    "cc3.a" -> "hello",
    "cc.f" -> "2, 3",
    "cc.dur" -> "Inf",
    "cc.findur" -> "22 days",
    "ccimpl2.a" -> "sdfds",
    "ccimpl2.b" -> "34",
    "thingimpl.t" -> "sadfsf"
  )

  println(decode[CC](config))
  println(decode[CC, Future, Throwable](config))
  println(decodeIO[CC](config))
  println(decodeIO[CC, EitherThrowable, Throwable](config))
  println(decodeAsync[CC, Task](config))
  println(decodeAsync[CC, Task, EitherErrors, ValidationErrors](config))
}
