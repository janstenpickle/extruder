package extruder.effect

import cats.data.EitherT
import cats.effect.IO
import cats.effect.laws.util.{TestContext, TestInstances}
import cats.instances.either._
import cats.{Eq, MonadError}
import extruder.core.TestCommon
import extruder.effect.EitherTIOEffectSuite.EitherIO

import scala.util.{Failure, Success, Try}

trait EitherTIOEffectSuite extends ThrowableEffectSuite[EitherIO] with TestInstances {
  self: EffectSuite[EitherIO, Throwable] =>
  override def FF: MonadError[EitherIO, Throwable] = MonadError[EitherIO, Throwable]

  implicit val tc: TestContext = TestContext()

  implicit def eitherEq[A](implicit eq: Eq[A]): Eq[Either[Throwable, A]] = Eq.instance { (x, y) =>
    def toTry(a: Either[Throwable, A]): Try[A] = a.fold(Failure(_), Success(_))
    TestCommon.tryEq[A].eqv(toTry(x), toTry(y))
  }

  override implicit def feq[A](implicit eq: Eq[A]): Eq[EitherT[IO, Throwable, A]] = Eq.instance { (x, y) =>
    eqIO[Either[Throwable, A]].eqv(x.value, y.value)
  }

  override def compareErrors[A](f: EitherIO[A], v1: Option[Throwable], v2: Option[Throwable])(
    implicit e: Eq[A]
  ): Boolean = {
    val err: EitherIO[A] = (v1, v2) match {
      case (Some(e1), Some(_)) => EitherT[IO, Throwable, A](IO.pure(Left(e1)))
      case (Some(e1), None) => EitherT[IO, Throwable, A](IO.pure(Left(e1)))
      case (None, Some(e2)) => EitherT[IO, Throwable, A](IO.pure(Left(e2)))
      case _ => f
    }
    feq[A].eqv(f, err)
  }

  override def getError[A](fa: EitherIO[A]): Throwable = fa.value.unsafeRunSync().left.get
}

object EitherTIOEffectSuite {
  type EitherIO[A] = EitherT[IO, Throwable, A]
}
