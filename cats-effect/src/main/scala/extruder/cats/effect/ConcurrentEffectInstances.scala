package extruder.cats.effect

import cats.{Functor, MonadError}
import cats.data.EitherT
import cats.effect._
import cats.syntax.either._
import extruder.core.errorsToThrowable
import extruder.data.{ValidationErrors, ValidationT}

trait ConcurrentEffectInstances extends ConcurrentInstances with EffectInstances {
  implicit def effectValidationConcurrentEffect[F[_]: ConcurrentEffect](
    implicit c: Concurrent[EitherT[F, ValidationErrors, ?]]
  ): ConcurrentEffect[EffectValidation[F, ?]] =
    new EffectValidationConcurrentEffect[F] {
      override def F: Concurrent[EitherT[F, ValidationErrors, ?]] = Concurrent[EitherT[F, ValidationErrors, ?]]
      override def FF: MonadError[ValidationT[F, ?], Throwable] = MonadError[ValidationT[F, ?], Throwable]
      override protected def FFF: Functor[EitherT[F, ValidationErrors, ?]] = F

      override def FFFF: ConcurrentEffect[F] = ConcurrentEffect[F]
    }

  private[effect] trait EffectValidationConcurrentEffect[F[_]]
      extends ConcurrentEffect[EffectValidation[F, ?]]
      with EffectValidationConcurrent[F]
      with EffectValidationEffect[F] {
    implicit def FFFF: ConcurrentEffect[F]

    override def runCancelable[A](
      fa: EffectValidation[F, A]
    )(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[EffectValidation[F, ?]]] =
      FFFF
        .runCancelable(fa.a.value)(cb.compose(_.right.flatMap(_.leftMap(errorsToThrowable))))
        .map(ct => EffectValidation(EitherT.liftF(ct)(FFFF)))
  }
}
