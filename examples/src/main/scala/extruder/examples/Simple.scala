package extruder.examples

import java.net.URL

import cats.{Apply, Eval}
import cats.data.{EitherT, NonEmptyList}
import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import extruder.core._
import extruder.effect.{ExtruderAsync, ExtruderMonadError, ExtruderSync}
import extruder.system.{EnvironmentSource, SafeEnvironmentSource, SafeSystemPropertiesSource, SystemPropertiesSource}
import extruder.typesafe.{SafeTypesafeConfigSource, TypesafeConfigDecoder, TypesafeConfigEncoder, TypesafeConfigSource}

import scala.util.{Either, Try}

//import scala.concurrent.ExecutionContext.Implicits.global

import scala.collection.JavaConverters._

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
    //"cc.dur" -> "Inf",
    "cc.findur" -> "22 days",
    "ccimpl2.a" -> "sdfds",
    "ccimpl2.b" -> "34",
    "thingimpl.t" -> "sadfsf"
  )

  //implicitly[ExtruderSync[ValidationT[IO, ?]]]

  //println(ExtruderSync.validationTSync[IO])

  //implicit val derp = ExtruderAsync.validationTAsync[IO]

  //type Val[A] = ValidationT[IO, A]
  type Eit[A] = EitherT[IO, Throwable, A]
  type EitV[A] = EitherT[IO, ValidationErrors, A]

  implicitly[ExtruderAsync[Eit]]
  implicitly[ExtruderSync[Eit]]
  implicitly[ExtruderMonadError[Eit]]
  implicitly[ExtruderAsync[EitV]]

//  println(TypesafeConfigSource.traversableEncoder[Val, Int, Seq])
  println(TypesafeConfigSource.encode[Seq[Int]](List("232"), Seq(1, 3)))
  println(MapSource.decode[CC](List("232"), Map.empty[String, String]))
  implicitly[MapDecoder[Validation, Set[Int]]]

  implicitly[Parser[Set[Int]]]

// CombinedDecoder.anyDecoder[Validation, String, MapDecoder, TypesafeConfigDecoder]
//
  println(
    implicitly[CombinedDecoder.Aux[Validation, Int, MapDecoder, MapDecoder]]
      .read(List.empty[String], None, (Map.empty[String, String], Map.empty[String, String]))
  )

  new Combined[MapDecoder, MapDecoder].derp((Map.empty[String, String], Map.empty[String, String]))

  implicitly[ExtruderAsync[IO]]

  Parser.traversable[Int, Set]

  val f: (Unit, Unit) => Unit = (_, _) => ()

  implicitly[ExtruderMonadError[Either[ValidationErrors, ?]]]

//  println(
//    implicitly[ExtruderAsync[EitV]]
//      .ap2[Unit, Unit, Unit](EitherT[IO, ValidationErrors, (Unit, Unit) => Unit](IO.pure(Right(f))))(
//        EitherT[IO, ValidationErrors, Unit](IO.pure(Left(NonEmptyList.of(Missing("fdsfw"))))),
//        EitherT[IO, ValidationErrors, Unit](IO.pure(Left(NonEmptyList.of(Missing("fder3sfw")))))
//      )
//      .value
//      .unsafeRunSync()
//  )

  println(
    implicitly[ExtruderMonadError[Validation]]
      .ap2[Unit, Unit, Unit](Right(f))(
        Left[ValidationErrors, Unit](NonEmptyList.of(Missing("fdsfw"))),
        Left[ValidationErrors, Unit](NonEmptyList.of(Missing("fder3sfw")))
      )
  )

  // println(Apply[Either[ValidationErrors, ?]].ap2()(fa0, fb0))

//  println(
//    EnvironmentSource
//      .decode[List[String], Val]
//      .value
//      .unsafeRunSync()
//  )
  //println(MapDecoder.decode[Int, Val](config))
//  implicit val fut = ExtruderApplicativeError.fromMonadError[Future]

  //println(decode[CC, Future, Throwable](config))

  //implicitly[ExtruderApplicativeError[EitherT[Future, Throwable, ?], Throwable]]

//  ExtruderApplicativeError.fromMonadError[EitherT[Future, Throwable, ?]]

  //implicitly[Effect[EitherT[Future, Throwable, ?]]]
  //implicitly[ExtruderEffect[Future]]
  // implicitly[ExtruderAsync[IO]]
  // implicitly[ExtruderAsync[ValidationT[IO, ?]]]
//  implicitly[ExtruderAsync[Task]]
  // implicitly[Async[EitherT[IO, NonEmptyList[String], ?]]]
  //println(decode[CC, EitherT[Future, Throwable, ?], Throwable](config))
//  println(decodeIO[CC](config))
//  println(decodeIO[CC, EitherThrowable, Throwable](config))
//  println(decodeAsync[CC, Task](config))
//  println(decodeAsync[CC, Task, EitherErrors, ValidationErrors](config))
}
