package extruder.instances

import cats.Eq
import cats.syntax.validated._
import extruder.core.{Validation, ValidationErrors}
import extruder.effect.{ExtruderMonadErrorSpec, ValidationEffectSpec}

class ValidationExtruderMonadErrorSpec
    extends ExtruderMonadErrorSpec[Validation, ValidationErrors]
    with ValidationEffectSpec[Validation] {
  override implicit def feq[A](implicit eq: Eq[A]): Eq[Validation[A]] = Eq.fromUniversalEquals

  override def getError[A](fa: Validation[A]): ValidationErrors = fa.toEither.left.get

  override def compareErrors[A](f: Validation[A], v1: Option[ValidationErrors], v2: Option[ValidationErrors])(
    implicit e: Eq[A]
  ): Boolean = {
    val err: Validation[A] = (v1, v2) match {
      case (Some(e1), Some(e2)) => (e1 ++ e2.toList).invalid
      case (Some(e1), None) => e1.invalid
      case (None, Some(e2)) => e2.invalid
      case _ => f
    }
    feq[A].eqv(f, err)
  }
}
