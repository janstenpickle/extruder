package extruder.examples

import java.net.URL

import cats.data.Validated.Valid
import cats.data.{EitherT, NonEmptyList, Validated}
import cats.effect.{Effect, IO}
import cats.syntax.validated._
import cats.{Applicative, Apply}
import extruder.core.{ExtruderEffect, Missing, Validation, ValidationError, ValidationException, ValidationFailure}
import extruder.data.ValidationT
import monix.eval.Task

//import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Await
import scala.util.{Failure, Success}
import extruder.monix._

//import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

import fs2.interop.cats._



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
    //"cc.dur" -> "Inf",
    "cc.findur" -> "22 days",
    "ccimpl2.a" -> "sdfds",
    "ccimpl2.b" -> "34",
    "thingimpl.t" -> "sadfsf"
  )

  //println(decode[CC](config))

//  implicit val fut = ExtruderApplicativeError.fromMonadError[Future]

  //println(decode[CC, Future, Throwable](config))

  //implicitly[ExtruderApplicativeError[EitherT[Future, Throwable, ?], Throwable]]

//  ExtruderApplicativeError.fromMonadError[EitherT[Future, Throwable, ?]]


//  implicitly[Effect[EitherT[Future, Throwable, ?]]]
//  implicitly[ExtruderEffect[Future]]
//  implicitly[ExtruderEffect[IO]]
//  implicitly[ExtruderEffect[ValidationT[IO, ?]]]
  //implicitly[ExtruderEffect[Task]]
  implicitly[ExtruderEffect[Validation]]
  implicitly[Effect[fs2.Task]]
  //println(decode[CC, EitherT[Future, Throwable, ?], Throwable](config))
//  println(decodeIO[CC](config))
//  println(decodeIO[CC, EitherThrowable, Throwable](config))
//  println(decodeAsync[CC, Task](config))
//  println(decodeAsync[CC, Task, EitherErrors, ValidationErrors](config))
}
