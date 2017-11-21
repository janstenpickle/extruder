package extruder.effect

import cats.data.EitherT
import cats.effect.{Effect, IO}
import cats.{Applicative, Eq}
import extruder.core.{TestCommon, Validation, ValidationErrors}

import scala.util.Try

trait ValidationTIOEffectSpec extends ValidationTEffectSpec[IO] {
  self: EffectSpec[EitherT[IO, ValidationErrors, ?], ValidationErrors] =>

  override implicit def FF: Applicative[IO] = Effect[IO]

  override implicit def feq[A](implicit eq: Eq[A]): Eq[EitherT[IO, ValidationErrors, A]] = Eq.instance { (x, y) =>
    TestCommon
      .tryEq[Validation[A]](validationEq)
      .eqv(Try[Validation[A]](x.value.unsafeRunSync()), Try[Validation[A]](y.value.unsafeRunSync()))
  }

  override def getError[A](fa: EitherT[IO, ValidationErrors, A]): ValidationErrors = fa.value.unsafeRunSync().left.get
}
