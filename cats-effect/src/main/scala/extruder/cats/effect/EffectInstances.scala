package extruder.cats.effect

import cats.data.EitherT
import cats.effect.{Async, Effect, IO, SyncIO}
import cats.syntax.either._
import cats.syntax.functor._
import cats.{Functor, MonadError}
import extruder.core.ValidationErrorsToThrowable
import extruder.data.{ValidationErrors, ValidationT}

import scala.util.Either

trait EffectInstances extends AsyncInstances {
  implicit def effectValidationEffect[F[_]: Effect](
    implicit as: Async[EitherT[F, ValidationErrors, ?]],
    tt: ValidationErrorsToThrowable
  ): Effect[EffectValidation[F, ?]] = new EffectValidationEffect[F] {
    override def F: Async[EitherT[F, ValidationErrors, ?]] = Async[EitherT[F, ValidationErrors, ?]]
    override def FF: MonadError[ValidationT[F, ?], Throwable] = MonadError[ValidationT[F, ?], Throwable]
    override protected def FFF: Functor[EitherT[F, ValidationErrors, ?]] = F

    override def FFFF: Effect[F] = Effect[F]
    override def toThrowable: ValidationErrorsToThrowable = tt
  }

  private[effect] trait EffectValidationEffect[F[_]]
      extends Effect[EffectValidation[F, ?]]
      with EffectValidationAsync[F] {
    implicit def FFFF: Effect[F]
    def toThrowable: ValidationErrorsToThrowable

    override def runAsync[A](fa: EffectValidation[F, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
      FFFF.runAsync(fa.a.value)(cb.compose(_.right.flatMap(_.leftMap(toThrowable.convertErrors))))

    override def toIO[A](fa: EffectValidation[F, A]): IO[A] =
      FFFF.toIO(FFFF.rethrow(fa.a.value.map(_.leftMap(toThrowable.convertErrors))))
  }
}
