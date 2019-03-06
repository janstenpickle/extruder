package extruder.cats.effect

import cats.Functor
import cats.data.EitherT
import cats.effect.{Clock, Timer}
import extruder.data.ValidationErrors

import scala.concurrent.duration.FiniteDuration

trait TimerInstances extends ClockInstances {
  implicit def deriveEffectValidationTimer[F[_]: Functor: Clock](
    implicit timer: Timer[EitherT[F, ValidationErrors, ?]],
    clock: Clock[EitherT[F, ValidationErrors, ?]]
  ): Timer[EffectValidation[F, ?]] =
    new Timer[EffectValidation[F, ?]] {
      override val clock: Clock[EffectValidation[F, ?]] = deriveEffectValidationClock[F]

      override def sleep(duration: FiniteDuration): EffectValidation[F, Unit] =
        EffectValidation(timer.sleep(duration))
    }
}
