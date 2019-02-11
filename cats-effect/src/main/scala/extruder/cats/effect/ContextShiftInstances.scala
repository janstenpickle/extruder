package extruder.cats.effect

import cats.data.EitherT
import cats.effect.ContextShift
import extruder.data.ValidationErrors

import scala.concurrent.ExecutionContext

trait ContextShiftInstances {
  implicit def deriveEffectValidationContextShift[F[_]](
    implicit
    cs: ContextShift[EitherT[F, ValidationErrors, ?]]
  ): ContextShift[EffectValidation[F, ?]] =
    new ContextShift[EffectValidation[F, ?]] {
      def shift: EffectValidation[F, Unit] =
        EffectValidation(cs.shift)

      def evalOn[A](ec: ExecutionContext)(fa: EffectValidation[F, A]): EffectValidation[F, A] =
        EffectValidation(cs.evalOn(ec)(fa.a))
    }
}
