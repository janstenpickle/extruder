package extruder.instances

import cats.data.EitherT
import cats.effect.IO
import cats.instances.all._
import cats.{Eq, MonadError}
import extruder.core.TestCommon
import extruder.effect.{ExtruderAsyncSpec, ThrowableEffectSpec}
import extruder.instances.EitherTIOExtruderAsyncSpec._

import scala.util.Try

class EitherTIOExtruderAsyncSpec extends ExtruderAsyncSpec[EitherIO, Throwable] with ThrowableEffectSpec[EitherIO] {
  override def FF: MonadError[EitherIO, Throwable] = MonadError[EitherIO, Throwable]

  override implicit def feq[A](implicit eq: Eq[A]): Eq[EitherT[IO, Throwable, A]] = Eq.instance { (x, y) =>
    TestCommon.tryEq[Either[Throwable, A]].eqv(Try(x.value.unsafeRunSync()), Try(y.value.unsafeRunSync()))
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

object EitherTIOExtruderAsyncSpec {
  type EitherIO[A] = EitherT[IO, Throwable, A]
}
