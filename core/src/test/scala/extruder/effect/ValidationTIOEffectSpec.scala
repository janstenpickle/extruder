package extruder.effect

import cats.effect.{Effect, IO}
import cats.{Applicative, Eq}
import extruder.core.{TestCommon, Validation, ValidationErrors}
import extruder.data.ValidationT

import scala.util.Try

trait ValidationTIOEffectSpec extends ValidationTEffectSpec[IO] {
  self: EffectSpec[ValidationT[IO, ?], ValidationErrors] =>

  override implicit def FF: Applicative[IO] = Effect[IO]

  override implicit def feq[A](implicit eq: Eq[A]): Eq[ValidationT[IO, A]] = Eq.instance { (x, y) =>
    TestCommon
      .tryEq[Validation[A]](validationEq)
      .eqv(Try[Validation[A]](x.value.unsafeRunSync()), Try[Validation[A]](y.value.unsafeRunSync()))
  }

  override def getError[A](fa: ValidationT[IO, A]): ValidationErrors = fa.value.unsafeRunSync().toEither.left.get
}
