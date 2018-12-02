package extruder.cats.effect

import cats.Functor
import cats.data.EitherT
import cats.effect.{IO, LiftIO}
import extruder.data.ValidationErrors

trait LiftIOInstances {
  implicit def effectValidationLiftIO[F[_]: Functor: LiftIO]: LiftIO[EffectValidation[F, ?]] =
    new EffectValidationLiftIO[F] {
      override protected def F: LiftIO[EitherT[F, ValidationErrors, ?]] = LiftIO[EitherT[F, ValidationErrors, ?]]
      override protected def FFF: Functor[EitherT[F, ValidationErrors, ?]] = Functor[EitherT[F, ValidationErrors, ?]]
    }

  private[effect] trait EffectValidationLiftIO[F[_]] extends LiftIO[EffectValidation[F, ?]] {
    protected implicit def F: LiftIO[EitherT[F, ValidationErrors, ?]]
    protected def FFF: Functor[EitherT[F, ValidationErrors, ?]]

    override def liftIO[A](ioa: IO[A]): EffectValidation[F, A] = EffectValidation(F.liftIO(ioa))
  }
}
