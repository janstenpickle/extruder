package extruder.cats.effect

import cats.MonadError
import cats.data.EitherT
import cats.effect.{ExitCase, Sync}
import extruder.data.{ValidationErrors, ValidationT}

trait SyncInstances {
  implicit def effectValidationSync[F[_]: Sync]: Sync[EffectValidation[F, ?]] = new EffectValidationSync[F] {
    override def F: Sync[EitherT[F, ValidationErrors, ?]] = Sync[EitherT[F, ValidationErrors, ?]]
    override def FF: MonadError[ValidationT[F, ?], Throwable] = MonadError[ValidationT[F, ?], Throwable]
  }

  private[effect] trait EffectValidationSync[F[_]] extends Sync[EffectValidation[F, ?]] {
    implicit def F: Sync[EitherT[F, ValidationErrors, ?]]
    implicit def FF: MonadError[ValidationT[F, ?], Throwable]

    override def suspend[A](thunk: => EffectValidation[F, A]): EffectValidation[F, A] =
      EffectValidation(F.suspend(thunk.a))

    override def bracketCase[A, B](acquire: EffectValidation[F, A])(
      use: A => EffectValidation[F, B]
    )(release: (A, ExitCase[Throwable]) => EffectValidation[F, Unit]): EffectValidation[F, B] =
      EffectValidation(F.bracketCase(acquire.a)(use.andThen(_.a))((a, ec) => release(a, ec).a))

    override def raiseError[A](e: Throwable): EffectValidation[F, A] =
      EffectValidation(FF.raiseError(e).a)

    override def handleErrorWith[A](
      fa: EffectValidation[F, A]
    )(f: Throwable => EffectValidation[F, A]): EffectValidation[F, A] =
      EffectValidation(FF.handleErrorWith(ValidationT(fa.a))(f.andThen(a => ValidationT(a.a))).a)

    override def pure[A](x: A): EffectValidation[F, A] =
      EffectValidation(F.pure(x))

    override def flatMap[A, B](fa: EffectValidation[F, A])(f: A => EffectValidation[F, B]): EffectValidation[F, B] =
      EffectValidation(F.flatMap(fa.a)(f.andThen(_.a)))

    override def tailRecM[A, B](a: A)(f: A => EffectValidation[F, Either[A, B]]): EffectValidation[F, B] =
      EffectValidation(F.tailRecM[A, B](a)(f.andThen(_.a)))

    override def ap[A, B](ff: EffectValidation[F, A => B])(fa: EffectValidation[F, A]): EffectValidation[F, B] =
      EffectValidation(FF.ap(ValidationT(ff.a))(ValidationT(fa.a)).a)

    override def ap2[A, B, Z](
      ff: EffectValidation[F, (A, B) => Z]
    )(fa: EffectValidation[F, A], fb: EffectValidation[F, B]): EffectValidation[F, Z] =
      EffectValidation(FF.ap2(ValidationT(ff.a))(ValidationT(fa.a), ValidationT(fb.a)).a)
  }
}
