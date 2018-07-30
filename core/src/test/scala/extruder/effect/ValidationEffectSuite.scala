package extruder.effect

import cats.Eq
import cats.data.NonEmptyList
import extruder.core.{Missing, ValidationErrors, ValidationException, ValidationFailure}

trait ValidationEffectSuite[F[_]] { self: EffectSuite[F, ValidationErrors] =>
  override def missingValue(message: String): NonEmptyList[Missing] = NonEmptyList.of(Missing(message))

  override def validationFailureValue(message: String): NonEmptyList[ValidationFailure] =
    NonEmptyList.of(ValidationFailure(message))

  override def validationExceptionValue(message: String, th: Throwable): NonEmptyList[ValidationException] =
    NonEmptyList.of(ValidationException(message, th))

  override def errEq: Eq[ValidationErrors] = Eq.fromUniversalEquals
}
