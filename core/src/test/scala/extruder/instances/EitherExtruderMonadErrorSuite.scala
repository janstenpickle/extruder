package extruder.instances

import cats.Eq
import extruder.core.{Missing, ValidationError, ValidationException, ValidationFailure}
import extruder.effect.ExtruderMonadErrorSuite

class EitherExtruderMonadErrorSuite extends ExtruderMonadErrorSuite[Either[ValidationError, ?], ValidationError] {
  override implicit def feq[A](implicit eq: Eq[A]): Eq[Either[ValidationError, A]] = Eq.fromUniversalEquals

  override def errEq: Eq[ValidationError] = Eq.fromUniversalEquals

  override def missingValue(message: String): ValidationError = Missing(message)

  override def validationFailureValue(message: String): ValidationError = ValidationFailure(message)

  override def validationExceptionValue(message: String, th: Throwable): ValidationError =
    ValidationException(message, th)

  override def getError[A](fa: Either[ValidationError, A]): ValidationError = fa.left.get

  override def compareErrors[A](
    f: Either[ValidationError, A],
    v1: Option[ValidationError],
    v2: Option[ValidationError]
  )(implicit e: Eq[A]): Boolean = {
    val err: Either[ValidationError, A] = (v1, v2) match {
      case (Some(e1), Some(_)) => Left(e1)
      case (Some(e1), None) => Left(e1)
      case (None, Some(e2)) => Left(e2)
      case _ => f
    }
    feq[A].eqv(f, err)
  }
}
