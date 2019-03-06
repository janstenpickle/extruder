package extruder.cats.effect

import cats.data.EitherT
import cats.effect.Async
import cats.{Functor, MonadError}
import extruder.data.{ValidationErrors, ValidationT}

trait AsyncInstances extends LiftIOInstances with SyncInstances {
  implicit def effectValidationAsync[F[_]: Async]: Async[EffectValidation[F, ?]] = new EffectValidationAsync[F] {
    override def F: Async[EitherT[F, ValidationErrors, ?]] = Async[EitherT[F, ValidationErrors, ?]]
    override def FF: MonadError[ValidationT[F, ?], Throwable] = MonadError[ValidationT[F, ?], Throwable]
    override protected def FFF: Functor[EitherT[F, ValidationErrors, ?]] = F
  }

  private[effect] trait EffectValidationAsync[F[_]]
      extends Async[EffectValidation[F, ?]]
      with EffectValidationLiftIO[F]
      with EffectValidationSync[F] {
    implicit def F: Async[EitherT[F, ValidationErrors, ?]]

    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): EffectValidation[F, A] =
      EffectValidation(F.async(k))

    override def asyncF[A](k: (Either[Throwable, A] => Unit) => EffectValidation[F, Unit]): EffectValidation[F, A] =
      EffectValidation(F.asyncF(cb => F.as(k(cb).a, ())))
  }
}
