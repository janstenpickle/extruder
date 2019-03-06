package extruder.cats.effect

import cats.data.EitherT
import cats.{Functor, MonadError}
import cats.effect.{CancelToken, Concurrent, Fiber => CFiber}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.either._
import extruder.data.{ValidationErrors, ValidationT}

trait ConcurrentInstances extends AsyncInstances {
  implicit def effectValidationConcurrent[F[_]: Concurrent]: Concurrent[EffectValidation[F, ?]] =
    new EffectValidationConcurrent[F] {
      override def F: Concurrent[EitherT[F, ValidationErrors, ?]] = Concurrent[EitherT[F, ValidationErrors, ?]]
      override def FF: MonadError[ValidationT[F, ?], Throwable] = MonadError[ValidationT[F, ?], Throwable]
      override protected def FFF: Functor[EitherT[F, ValidationErrors, ?]] = F
      override def FFFF: Functor[F] = Functor[F]
    }

  private[effect] trait EffectValidationConcurrent[F[_]]
      extends Concurrent[EffectValidation[F, ?]]
      with EffectValidationAsync[F] {
    implicit def F: Concurrent[EitherT[F, ValidationErrors, ?]]
    implicit def FFFF: Functor[F]

    // Needed to drive static checks, otherwise the
    // compiler will choke on type inference :-(
    type Fiber[A] = cats.effect.Fiber[EffectValidation[F, ?], A]

    override def cancelable[A](
      k: (Either[Throwable, A] => Unit) => CancelToken[EffectValidation[F, ?]]
    ): EffectValidation[F, A] =
      EffectValidation(F.cancelable(k.andThen(_.a)))

    override def start[A](fa: EffectValidation[F, A]): EffectValidation[F, Fiber[A]] =
      EffectValidation(F.start(fa.a).map(fiberMap))

    override def racePair[A, B](
      fa: EffectValidation[F, A],
      fb: EffectValidation[F, B]
    ): EffectValidation[F, Either[(A, Fiber[B]), (Fiber[A], B)]] =
      EffectValidation(
        F.racePair(fa.a, fb.a)
          .map(_.bimap({ case (a, fiberB) => a -> fiberMap(fiberB) }, { case (fiberA, b) => fiberMap(fiberA) -> b }))
      )

    private def fiberMap[A](fiber: CFiber[EitherT[F, ValidationErrors, ?], A]): Fiber[A] =
      CFiber[EffectValidation[F, ?], A](EffectValidation(fiber.join), EffectValidation(fiber.cancel))
  }
}
