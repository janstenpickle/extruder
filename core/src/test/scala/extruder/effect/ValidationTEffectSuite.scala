package extruder.effect

import cats.data.EitherT
import cats.{Applicative, Eq}
import extruder.core.{Validation, ValidationErrors}

trait ValidationTEffectSuite[F[_]] extends ValidationEffectSuite[EitherT[F, ValidationErrors, ?]] {
  self: EffectSuite[EitherT[F, ValidationErrors, ?], ValidationErrors] =>

  implicit def FF: Applicative[F]

  implicit def validationEq[A]: Eq[Validation[A]] = Eq.fromUniversalEquals

  override def compareErrors[A](
    f: EitherT[F, ValidationErrors, A],
    v1: Option[ValidationErrors],
    v2: Option[ValidationErrors]
  )(implicit e: Eq[A]): Boolean = {
    val err: EitherT[F, ValidationErrors, A] = (v1, v2) match {
      case (Some(e1), Some(e2)) => EitherT.leftT(e1 ++ e2.toList)
      case (Some(e1), None) => EitherT.leftT(e1)
      case (None, Some(e2)) => EitherT.leftT(e2)
      case _ => f
    }
    feq[A].eqv(f, err)
  }
}
