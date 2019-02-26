package extruder.examples

import java.net.URL

import cats.effect.IO
import cats.instances.future._
import cats.instances.try_._
import eu.timepit.refined.types.numeric.PosInt
import extruder.cats.effect.{EffectValidation, EvalValidation}
import extruder.map._
import extruder.refined._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try

case class CC(
  a: String = "test",
  b: String = "test2",
  c: PosInt,
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
    "cc.d.cc2.dfs" -> "100",
    "cc3.a" -> "hello",
    "cc.f" -> "2, 3",
    "cc.dur" -> "Inf",
    "cc.findur" -> "22 days",
    "ccimpl2.a" -> "sdfds",
    "ccimpl2.b" -> "34",
    "thingimpl.t" -> "sadfsf"
  )

  val config1 = Map(
    "cc.a" -> "sdfsf",
    "cc.e.cc3.a" -> "test3",
    "cc.d.cc2.z.cc3.a" -> "testing",
    "cc.d.cc2.dfs" -> "100",
    "cc3.a" -> "hello",
    "cc.f" -> "2, 3",
    "cc.dur" -> "Inf",
    "cc.findur" -> "22 days",
    "thingimpl.t" -> "sadfsf"
  )

  val config2 = Map(
    "cc.c" -> "2000",
    "cc.a" -> "sdfsf",
    "cc.e.cc3.a" -> "test3",
    "cc.d.cc2.z.cc3.a" -> "testing",
    "cc.d.cc2.dfs" -> "100",
    "cc3.a" -> "hello",
    "cc.dur" -> "Inf",
    "cc.findur" -> "22 days",
    "ccimpl2.a" -> "sdfds",
    "ccimpl2.b" -> "34",
    "thingimpl.t" -> "sadfsf"
  )

  type Ev[A] = EffectValidation[IO, A]

  println(encode[Sealed](ObjImpl))

  EncoderT[Validation, Se]

//  println(decode[CC].combine(decode[CC])((config1, config2)))
//  println(encode.combine(encode)(CC3(Some("xzf"))))
//  println(decode[CC](Map.empty[String, String]))
//  println(decodeF[Try, CC](config))
//  println(decodeF[Future, CC](config))
//  println(decodeF[IO, CC](config).unsafeRunSync())
//  println(decodeF[Ev, CC](config).value.unsafeRunSync())
//  println(decodeF[EvalValidation, CC](Map.empty[String, String]).value.value)
}
