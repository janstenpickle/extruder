package extruder.effect

import cats.effect.IO
import cats.effect.laws.util.TestContext
import cats.effect.laws.util.TestInstances._
import cats.{Eq, MonadError}

import scala.util.Try

trait IOEffectSpec extends ThrowableEffectSuite[IO] { self: EffectSuite[IO, Throwable] =>
  override def FF: MonadError[IO, Throwable] = MonadError[IO, Throwable]

  implicit val tc: TestContext = TestContext()

  override def feq[A](implicit eq: Eq[A]): Eq[IO[A]] = eqIO[A]

  override def getError[A](fa: IO[A]): Throwable = Try(fa.unsafeRunSync()).failed.get
}
