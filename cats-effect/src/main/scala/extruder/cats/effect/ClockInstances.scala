package extruder.cats.effect

import cats.Functor
import cats.data.EitherT
import cats.effect.Clock
import extruder.data.ValidationErrors

import scala.concurrent.duration.TimeUnit

trait ClockInstances {
  implicit def deriveEffectValidationClock[F[_]: Functor](
    implicit clock: Clock[EitherT[F, ValidationErrors, ?]]
  ): Clock[EffectValidation[F, ?]] =
    new Clock[EffectValidation[F, ?]] {
      override def realTime(unit: TimeUnit): EffectValidation[F, Long] =
        EffectValidation(clock.realTime(unit))

      override def monotonic(unit: TimeUnit): EffectValidation[F, Long] =
        EffectValidation(clock.monotonic(unit))
    }
}
