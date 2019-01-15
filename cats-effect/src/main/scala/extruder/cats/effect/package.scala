package extruder.cats

import cats.data.EitherT
import cats.effect.ExitCase.{Completed, Error}
import cats.effect._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.{Eval, Monad, MonadError}
import extruder.core.{ExtruderErrors, ValidationErrorsToThrowable}
import extruder.data._
import io.estatico.newtype.macros.newsubtype

import scala.util.control.NonFatal

package object effect {
  @newsubtype case class EvalValidation[A](a: EitherT[Eval, ValidationErrors, A])

  object EvalValidation extends EvalValidationLowPriorityInstances {
    implicit def extruderErrorsEvalValidation: ExtruderErrors[EvalValidation] = new ExtruderErrors[EvalValidation] {
      override def missing[A](message: String): EvalValidation[A] =
        EvalValidation(EitherT.leftT[Eval, A](ValidationErrors.missing(message)))
      override def validationFailure[A](message: String): EvalValidation[A] =
        EvalValidation(EitherT.leftT[Eval, A](ValidationErrors.failure(message)))
      override def validationException[A](message: String, ex: Throwable): EvalValidation[A] =
        EvalValidation(EitherT.leftT[Eval, A](ValidationErrors.exception(message, ex)))
      override def fallback[A](fa: EvalValidation[A])(thunk: => EvalValidation[A]): EvalValidation[A] =
        EvalValidation(EitherT(fa.a.value.flatMap(_.fold(_ => thunk.a.value, a => Eval.later(Right(a))))))
    }
  }

  trait EvalValidationLowPriorityInstances {
    implicit def extruderStdInstancesForEvalValidation(
      implicit toThrowable: ValidationErrorsToThrowable
    ): Sync[EvalValidation] = new Sync[EvalValidation] {
      def F: MonadError[ValidationT[Eval, ?], Throwable] = MonadError[ValidationT[Eval, ?], Throwable]
      def FF: Monad[EitherT[Eval, ValidationErrors, ?]] =
        Monad[EitherT[Eval, ValidationErrors, ?]]

      override def suspend[A](thunk: => EvalValidation[A]): EvalValidation[A] =
        EvalValidation(EitherT(Eval.later(try {
          thunk.a.value.value
        } catch {
          case NonFatal(th) => Left(ValidationErrors.exception(th))
        })))

      override def delay[A](thunk: => A): EvalValidation[A] =
        EvalValidation(EitherT[Eval, ValidationErrors, A](Eval.always(try {
          Right(thunk)
        } catch {
          case NonFatal(th) => Left(ValidationErrors.exception(th))
        })))

      override def bracketCase[A, B](
        acquire: EvalValidation[A]
      )(use: A => EvalValidation[B])(release: (A, ExitCase[Throwable]) => EvalValidation[Unit]): EvalValidation[B] =
        acquire.a.value.value match {
          case Right(a) =>
            val res = use(a)
            res.a.value.value match {
              case Right(b) => EvalValidation(release(a, Completed).a.map(_ => b))
              case Left(errs) =>
                release(a, Error(toThrowable.convertErrors(errs))).a.value.value match {
                  case Right(_) => res
                  case Left(_) => res
                }
            }
          case e @ Left(_) => EvalValidation(EitherT(Eval.now(e.rightCast[B])))
        }

      override def raiseError[A](e: Throwable): EvalValidation[A] =
        EvalValidation(F.raiseError(e).a)

      override def handleErrorWith[A](fa: EvalValidation[A])(f: Throwable => EvalValidation[A]): EvalValidation[A] =
        EvalValidation(F.handleErrorWith(ValidationT(fa.a))(f.andThen(a => ValidationT(a.a))).a)

      override def pure[A](x: A): EvalValidation[A] = EvalValidation(FF.pure(x))
      override def flatMap[A, B](fa: EvalValidation[A])(f: A => EvalValidation[B]): EvalValidation[B] =
        EvalValidation(FF.flatMap(fa.a)(f.andThen(_.a)))
      override def tailRecM[A, B](a: A)(f: A => EvalValidation[Either[A, B]]): EvalValidation[B] =
        EvalValidation(FF.tailRecM(a)(f.andThen(_.a)))

      override def ap[A, B](ff: EvalValidation[A => B])(fa: EvalValidation[A]): EvalValidation[B] =
        EvalValidation(F.ap(ValidationT(ff.a))(ValidationT(fa.a)).a)

      override def ap2[A, B, Z](
        ff: EvalValidation[(A, B) => Z]
      )(fa: EvalValidation[A], fb: EvalValidation[B]): EvalValidation[Z] =
        EvalValidation(F.ap2(ValidationT(ff.a))(ValidationT(fa.a), ValidationT(fb.a)).a)
    }
  }

  @newsubtype case class EffectValidation[F[_], A](a: EitherT[F, ValidationErrors, A])

  object EffectValidation extends TimerInstances with ContextShiftInstances with ConcurrentEffectInstances {
    implicit def extruderErrorsEffectValidation[F[_]](
      implicit F: MonadError[F, Throwable]
    ): ExtruderErrors[EffectValidation[F, ?]] =
      new ExtruderErrors[EffectValidation[F, ?]] {
        override def missing[A](message: String): EffectValidation[F, A] =
          EffectValidation(EitherT.leftT[F, A](ValidationErrors.missing(message)))
        override def validationFailure[A](message: String): EffectValidation[F, A] =
          EffectValidation(EitherT.leftT[F, A](ValidationErrors.failure(message)))
        override def validationException[A](message: String, ex: Throwable): EffectValidation[F, A] =
          EffectValidation(EitherT.leftT[F, A](ValidationErrors.exception(message, ex)))
        override def fallback[A](fa: EffectValidation[F, A])(thunk: => EffectValidation[F, A]): EffectValidation[F, A] =
          EffectValidation(EitherT(fa.a.value.flatMap(_.fold(_ => thunk.a.value, a => F.pure(Right(a))))))
      }
  }
}
