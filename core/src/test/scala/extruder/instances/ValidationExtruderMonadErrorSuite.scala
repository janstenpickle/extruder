package extruder.instances

import cats.Eq
import extruder.core.{Validation, ValidationErrors}
import extruder.effect.{ExtruderMonadErrorSuite, ValidationEffectSuite}

class ValidationExtruderMonadErrorSuite
    extends ExtruderMonadErrorSuite[Validation, ValidationErrors]
    with ValidationEffectSuite[Validation] {
  override implicit def feq[A](implicit eq: Eq[A]): Eq[Validation[A]] = Eq.fromUniversalEquals

  override def getError[A](fa: Validation[A]): ValidationErrors = fa.left.get

  override def compareErrors[A](f: Validation[A], v1: Option[ValidationErrors], v2: Option[ValidationErrors])(
    implicit e: Eq[A]
  ): Boolean = {
    val err: Validation[A] = (v1, v2) match {
      case (Some(e1), Some(e2)) => Left(e1 ++ e2.toList)
      case (Some(e1), None) => Left(e1)
      case (None, Some(e2)) => Left(e2)
      case _ => f
    }
    feq[A].eqv(f, err)
  }
}
