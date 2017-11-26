package extruder.effect

import cats.{Eq, MonadError}
import cats.effect.IO
import extruder.core.TestCommon

import scala.util.Try

trait IOEffectSpec extends ThrowableEffectSpec[IO] { self: EffectSpec[IO, Throwable] =>
  override def FF: MonadError[IO, Throwable] = MonadError[IO, Throwable]

  override implicit def feq[A](implicit eq: Eq[A]): Eq[IO[A]] =
    Eq.instance((x, y) => TestCommon.tryEq[A].eqv(Try(x.unsafeRunSync()), Try(y.unsafeRunSync())))

  override def getError[A](fa: IO[A]): Throwable = Try(fa.unsafeRunSync()).failed.get
}
