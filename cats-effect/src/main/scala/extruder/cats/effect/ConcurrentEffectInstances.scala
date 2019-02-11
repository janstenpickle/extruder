package extruder.cats.effect

import cats.{Functor, MonadError}
import cats.data.EitherT
import cats.effect._
import cats.syntax.either._
import extruder.core.ValidationErrorsToThrowable
import extruder.data.{ValidationErrors, ValidationT}

trait ConcurrentEffectInstances extends ConcurrentInstances with EffectInstances {
  implicit def effectValidationConcurrentEffect[F[_]: ConcurrentEffect](
    implicit c: Concurrent[EitherT[F, ValidationErrors, ?]],
    tt: ValidationErrorsToThrowable
  ): ConcurrentEffect[EffectValidation[F, ?]] =
    new EffectValidationConcurrentEffect[F] {
      override def F: Concurrent[EitherT[F, ValidationErrors, ?]] = Concurrent[EitherT[F, ValidationErrors, ?]]
      override def FF: MonadError[ValidationT[F, ?], Throwable] = MonadError[ValidationT[F, ?], Throwable]
      override protected def FFF: Functor[EitherT[F, ValidationErrors, ?]] = F

      override def FFFF: ConcurrentEffect[F] = ConcurrentEffect[F]
      override def toThrowable: ValidationErrorsToThrowable = tt
    }

  private[effect] trait EffectValidationConcurrentEffect[F[_]]
      extends ConcurrentEffect[EffectValidation[F, ?]]
      with EffectValidationConcurrent[F]
      with EffectValidationEffect[F] {
    implicit def FFFF: ConcurrentEffect[F]
    def toThrowable: ValidationErrorsToThrowable

    override def runCancelable[A](
      fa: EffectValidation[F, A]
    )(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[EffectValidation[F, ?]]] =
      FFFF
        .runCancelable(fa.a.value)(cb.compose(_.right.flatMap(_.leftMap(toThrowable.convertErrors))))
        .map(ct => EffectValidation(EitherT.liftF(ct)(FFFF)))
  }
}
