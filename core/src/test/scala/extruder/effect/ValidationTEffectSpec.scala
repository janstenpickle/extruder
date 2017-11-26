package extruder.effect

import cats.{Applicative, Eq}
import cats.syntax.validated._
import extruder.core.{Validation, ValidationErrors}
import extruder.data.ValidationT

trait ValidationTEffectSpec[F[_]] extends ValidationEffectSpec[ValidationT[F, ?]] {
  self: EffectSpec[ValidationT[F, ?], ValidationErrors] =>

  implicit def FF: Applicative[F]

  implicit def validationEq[A]: Eq[Validation[A]] = Eq.fromUniversalEquals

  override def compareErrors[A](f: ValidationT[F, A], v1: Option[ValidationErrors], v2: Option[ValidationErrors])(
    implicit e: Eq[A]
  ): Boolean = {
    val err: ValidationT[F, A] = (v1, v2) match {
      case (Some(e1), Some(e2)) => ValidationT.lift[F, A]((e1 ++ e2.toList).invalid)
      case (Some(e1), None) => ValidationT.lift[F, A](e1.invalid)
      case (None, Some(e2)) => ValidationT.lift[F, A](e2.invalid)
      case _ => f
    }
    feq[A].eqv(f, err)
  }
}
