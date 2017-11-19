package extruder.instances

import cats.effect.{Effect, IO}
import cats.{Applicative, Eq}
import extruder.core.{TestCommon, Validation, ValidationErrors}
import extruder.data.ValidationT
import extruder.effect.{ExtruderAsync, ExtruderAsyncSpec, ValidationTEffectSpec}
import extruder.instances.ValidationTIOExtruderAsyncSpec._

import scala.util.Try

class ValidationTIOExtruderAsyncSpec
    extends ExtruderAsyncSpec[IOVal, ValidationErrors]
    with ValidationTEffectSpec[IO] {
  override implicit def FF: Applicative[IO] = Effect[IO]

  override implicit def feq[A](implicit eq: Eq[A]): Eq[ValidationT[IO, A]] = Eq.instance { (x, y) =>
    TestCommon
      .tryEq[Validation[A]](validationEq)
      .eqv(Try[Validation[A]](x.value.unsafeRunSync()), Try[Validation[A]](y.value.unsafeRunSync()))
  }

  override def getError[A](fa: ValidationT[IO, A]): ValidationErrors = fa.value.unsafeRunSync().toEither.left.get
}

object ValidationTIOExtruderAsyncSpec {
  type IOVal[A] = ValidationT[IO, A]

  implicit val Instance: ExtruderAsync[IOVal] = ExtruderAsync.validationTAsync[IO]
}
