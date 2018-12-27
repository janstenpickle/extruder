package extruder.examples

import java.net.URL

import cats.{Applicative, Id}
import cats.data.{EitherT, Ior, NonEmptyList}
import cats.effect.{IO, Timer}
import com.typesafe.config.ConfigFactory
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.types.numeric.PosInt
import extruder.core.DecoderT
import extruder.data.Transform
import extruder.meta.{MetaInfo, Primitive, ReprTable}
//import cats.instances.all._
import extruder.cats.effect.{EffectValidation, EvalValidation}
import extruder.core.{EncoderT, Settings}
//import extruder.core.MapSource.{decode, decodeF, encode, encodeF}
import extruder.data.{StringWriter, Validation, ValidationErrors, ValidationT}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try
import cats.syntax.applicative._

import eu.timepit.refined.collection.NonEmpty

import com.typesafe.config.Config

//import extruder.map._

import extruder.typesafe._
import extruder.map.instances._
import extruder.refined._

import scala.collection.JavaConverters._

case class CC(
  a: String = "test",
  b: String = "test2",
  c: PosInt,
  d: CC2,
  e: CC3,
  f: Set[Int],
  dur: Duration,
  //  finDur: FiniteDuration
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

  type Ev[A] = EffectValidation[IO, A]

  //println(decode[CC](Map.empty[String, String]))
  // println(decode[Try, CC](config))
//  println(decode[Future, CC](config))
//  println(decode[IO, CC](config))
  //println(decodeF[Ev, CC](config).value.unsafeRunSync())
//  printlntln(decodeF[EvalValidation, CC](Map.empty).value.value)
//  println(encode("sdfs"))

//  EncoderT[Id, Settings, String, Map[String, String]]
//
//  DecoderT[Validation, Settings, String, Map[String, String]]
//
//  DecoderT.parserDecoder[Validation, String, Settings, Map[String, String]]

  println(encode(Option(34)))

  //println(decode[Option[String]](Map("" -> "sdfds")))

//   val x: Transform[
//    Validation,
//    (Settings, Settings),
//    Ior[Map[String, String], IntermediateTypes.Config],
//    Ior[Map[String, String], Config]
//  ] = EncoderT
//    .iorCombinedTransform[Validation, Settings, Settings, Map[String, String], EncodeData, Map[String, String], Config]

//  println(encodeCombinedF[Validation](extruder.map.datasource)(CC3(Some("dfds"))))

//  println(
//    extruder.map.datasource
//      .decodeF[Validation, CC]
//      .combine(extruder.map.datasource)((config, config.updated("cc.c", "ewfwnfwoef")))
//  )

  println(ReprTable.asTable[CC](List.empty, defaultSettings))

  implicitly[MetaInfo[NonEmptyList[Int]]]

  implicitly[Primitive[String]]

  implicitly[MetaInfo[NonEmptyString]]

  implicit val timer = IO.timer(ExecutionContext.global)
}
