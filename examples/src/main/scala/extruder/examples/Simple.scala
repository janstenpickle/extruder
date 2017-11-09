package extruder.examples

import java.net.URL

import cats.{Applicative, Apply, Monad, MonadError}
import cats.data.{EitherT, NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{Async, Effect, IO}
import extruder.core.MapSource._
import extruder.core.{
  EitherErrors,
  EitherThrowable,
  ExtruderApplicativeError,
  ExtruderEffect,
  Missing,
  Validation,
  ValidationError,
  ValidationErrors
}
import extruder.instances.ValidationT
import extruder.monix._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import cats.syntax.validated._

import scala.concurrent.Await

//import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import cats.instances.all._

import cats.effect.Effect._

import scala.util.Try

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

  implicit val futEffect: Effect[Future] = new Effect[Future] {
    override def runAsync[A](fa: Future[A])(cb: (Either[Throwable, A]) => IO[Unit]): IO[Unit] = ???

    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): Future[A] = ???

    override def suspend[A](thunk: => Future[A]): Future[A] = ???

    override def tailRecM[A, B](a: A)(f: (A) => Future[Either[A, B]]): Future[B] = ???

    override def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = ???

    override def raiseError[A](e: Throwable): Future[A] = ???

    override def handleErrorWith[A](fa: Future[A])(f: (Throwable) => Future[A]): Future[A] = ???

    override def pure[A](x: A): Future[A] = ???
  }

  implicit val validationEffect: Effect[Validation] = new Effect[Validation] {
    override def runAsync[A](fa: Validation[A])(cb: (Either[Throwable, A]) => IO[Unit]): IO[Unit] = ???

    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): Validation[A] = ???

    override def suspend[A](thunk: => Validation[A]): Validation[A] = ???

    override def tailRecM[A, B](a: A)(f: (A) => Validation[Either[A, B]]): Validation[B] = ???

    override def flatMap[A, B](fa: Validation[A])(f: (A) => Validation[B]): Validation[B] = fa.fold(_.invalid, f)

    override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validated[NonEmptyList[ValidationError], B] =
      fa.ap(ff)

    override def raiseError[A](e: Throwable): Validation[A] = ???

    override def handleErrorWith[A](fa: Validation[A])(f: (Throwable) => Validation[A]): Validation[A] = ???

    override def product[A, B](fa: Validation[A], fb: Validation[B]): Validation[(A, B)] = fa.product(fb)

    override def pure[A](x: A): Validation[A] = Valid(x)
  }

  val derp = new Effect[ValidationT[Future, ?]] {
    override def runAsync[A](fa: ValidationT[Future, A])(cb: (Either[Throwable, A]) => IO[Unit]) = ???

    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit) = ???

    override def suspend[A](thunk: => ValidationT[Future, A]) = ???

    override def flatMap[A, B](fa: ValidationT[Future, A])(f: (A) => ValidationT[Future, B]) = ???

    override def tailRecM[A, B](a: A)(f: (A) => ValidationT[Future, Either[A, B]]) = ???

    override def raiseError[A](e: Throwable) = ???

    override def handleErrorWith[A](fa: ValidationT[Future, A])(f: (Throwable) => ValidationT[Future, A]) = ???

    override def ap2[A, B, Z](
      ff: ValidationT[Future, (A, B) => Z]
    )(fa: ValidationT[Future, A], fb: ValidationT[Future, B]): ValidationT[Future, Z] =
      ValidationT[Future, Z](for {
        faa <- fa.value
        fbb <- fb.value
        fff <- ff.value
      } yield Apply[Validation].ap2(fff)(faa, fbb))

    //super.ap2(ff)(fa, fb)

    override def pure[A](x: A): ValidationT[Future, A] = ValidationT[Future, A](Future.successful(Valid(x)))
  }

  println(
    validationEffect
      .ap2[Unit, Unit, Unit](((a: Unit, b: Unit) => ()).validNel)(
        Missing("34").invalidNel[Unit],
        Missing("dsfdsf").invalidNel[Unit]
      )
  )

  println(
    Await.result(
      implicitly[ExtruderEffect[ValidationT[IO, ?]]]
        .ap2[Unit, Unit, Unit](ValidationT[IO, (Unit, Unit) => Unit](IO.pure(((a: Unit, b: Unit) => ()).validNel)))(
          ValidationT[IO, Unit](IO.pure(Missing("34").invalidNel[Unit])),
          ValidationT[IO, Unit](IO.pure(Missing("dsfdsf").invalidNel[Unit]))
        )
        .value
        .unsafeToFuture(),
      Duration.Inf
    )
  )

  implicitly[Effect[EitherT[Future, Throwable, ?]]]
  implicitly[ExtruderEffect[Future]]
  implicitly[ExtruderEffect[IO]]
  implicitly[ExtruderEffect[ValidationT[IO, ?]]]
  //println(decode[CC, EitherT[Future, Throwable, ?], Throwable](config))
//  println(decodeIO[CC](config))
//  println(decodeIO[CC, EitherThrowable, Throwable](config))
//  println(decodeAsync[CC, Task](config))
//  println(decodeAsync[CC, Task, EitherErrors, ValidationErrors](config))
}
