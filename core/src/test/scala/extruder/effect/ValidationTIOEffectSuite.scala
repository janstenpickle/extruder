package extruder.effect

import cats.data.EitherT
import cats.effect.laws.util.{TestContext, TestInstances}
import cats.effect.{Effect, IO}
import cats.{Applicative, Eq}
import extruder.core.ValidationErrors

trait ValidationTIOEffectSuite extends ValidationTEffectSuite[IO] with TestInstances {
  self: EffectSuite[EitherT[IO, ValidationErrors, ?], ValidationErrors] =>

  implicit val tc: TestContext = TestContext()

  override implicit def FF: Applicative[IO] = Effect[IO]

  override implicit def feq[A](implicit eq: Eq[A]): Eq[EitherT[IO, ValidationErrors, A]] =
    Eq.instance { (x, y) =>
      eqIO[Either[ValidationErrors, A]].eqv(x.value, y.value)
    }

  override def getError[A](fa: EitherT[IO, ValidationErrors, A]): ValidationErrors = fa.value.unsafeRunSync().left.get
}
