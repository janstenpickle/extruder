package extruder

import _root_.cats.data.{EitherT, NonEmptyList}
import _root_.cats.instances.all._
import _root_.cats.syntax.either._
import _root_.cats.syntax.flatMap._
import _root_.cats.{Apply, Monad, MonadError}
import extruder.core.ExtruderErrors
import _root_.io.estatico.newtype.macros.newsubtype
import _root_.shapeless.LowPriority

package object data {
  type ValidationErrors = NonEmptyList[ValidationError]

  @newsubtype case class Validation[A](a: Either[ValidationErrors, A])

  object Validation {
    implicit val extruderStdInstancesForValidation: MonadError[Validation, Throwable] =
      new MonadError[Validation, Throwable] {
        def F: MonadError[Either[ValidationErrors, ?], ValidationErrors] =
          MonadError[Either[ValidationErrors, ?], ValidationErrors]

        override def raiseError[A](e: Throwable): Validation[A] =
          Validation(Left(NonEmptyList.of(ValidationException(e))))

        override def handleErrorWith[A](fa: Validation[A])(f: Throwable => Validation[A]): Validation[A] =
          fa.a.fold(e => f(extruder.core.errorsToThrowable(e)), a => Validation(Right(a)))

        override def flatMap[A, B](fa: Validation[A])(f: A => Validation[B]): Validation[B] =
          Validation(F.flatMap(fa.a)(f.andThen(_.a)))

        override def tailRecM[A, B](a: A)(f: A => Validation[Either[A, B]]): Validation[B] =
          Validation(F.tailRecM(a)(f.andThen(_.a)))

        override def pure[A](x: A): Validation[A] = Validation(F.pure(x))

        override def ap2[A, B, Z](ff: Validation[(A, B) => Z])(fa: Validation[A], fb: Validation[B]): Validation[Z] =
          apply.ap2(ff)(fa, fb)

        override def ap[A, B](ff: Validation[A => B])(fa: Validation[A]): Validation[B] =
          apply.ap(ff)(fa)

      }

    implicit val extruderErrorsForValidation: ExtruderErrors[Validation] = new ExtruderErrors[Validation] {
      override def missing[A](message: String): Validation[A] = Validation(Left(NonEmptyList.of(Missing(message))))
      override def validationFailure[A](message: String): Validation[A] =
        Validation(Left(NonEmptyList.of(ValidationFailure(message))))
      override def validationException[A](message: String, ex: Throwable): Validation[A] =
        Validation(Left(NonEmptyList.of(ValidationException(message, ex))))
      override def fallback[A](a: Validation[A])(thunk: => Validation[A]): Validation[A] =
        a.a.fold(_ => thunk, v => Validation(Right(v)))
    }

    protected val apply: Apply[Validation] = new Apply[Validation] {
      override def ap[A, B](ff: Validation[A => B])(fa: Validation[A]): Validation[B] =
        Validation((fa.a, ff.a) match {
          case (Right(a), Right(f)) => Right(f(a))
          case (Left(e1), Left(e2)) => Left(e2 ++ e1.toList)
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        })

      override def map[A, B](fa: Validation[A])(f: A => B): Validation[B] = Validation(fa.a.map(f))
    }
  }

  @newsubtype case class ValidationT[F[_], A](a: EitherT[F, ValidationErrors, A])

  object ValidationT extends LowPriorityValidationTInstances {

    implicit def extruderStdInstancesForValidationT[F[_]](
      implicit F: MonadError[F, Throwable]
    ): MonadError[ValidationT[F, ?], Throwable] = new ValidationTMonadError[F] {
      def FF: Monad[F] = F

      override def raiseError[A](e: Throwable): ValidationT[F, A] =
        ValidationT(EitherT[F, ValidationErrors, A](F.raiseError(e)))

      override def handleErrorWith[A](fa: ValidationT[F, A])(f: Throwable => ValidationT[F, A]): ValidationT[F, A] =
        ValidationT(EitherT(F.handleErrorWith(fa.a.value)(f.andThen(_.a.value))))
    }

  }

  trait LowPriorityValidationTInstances {
    implicit def extruderStdInstancesForValidationTFromMonad[F[_]](
      implicit F: Monad[F]
    ): MonadError[ValidationT[F, ?], Throwable] = new ValidationTMonadError[F] {
      def FF: Monad[F] = F
    }

    trait ValidationTMonadError[F[_]] extends MonadError[ValidationT[F, ?], Throwable] {
      implicit def FF: Monad[F]

      def FFF: MonadError[EitherT[F, ValidationErrors, ?], ValidationErrors] =
        MonadError[EitherT[F, ValidationErrors, ?], ValidationErrors]

      override def pure[A](x: A): ValidationT[F, A] = ValidationT(FFF.pure(x))
      override def flatMap[A, B](fa: ValidationT[F, A])(f: A => ValidationT[F, B]): ValidationT[F, B] =
        ValidationT(FFF.flatMap(fa.a)(f.andThen(_.a)))

      override def tailRecM[A, B](a: A)(f: A => ValidationT[F, Either[A, B]]): ValidationT[F, B] =
        ValidationT(FFF.tailRecM(a)(f.andThen(_.a)))

      override def raiseError[A](e: Throwable): ValidationT[F, A] =
        ValidationT(FFF.raiseError(NonEmptyList.of(ValidationException(e))))

      override def handleErrorWith[A](fa: ValidationT[F, A])(f: Throwable => ValidationT[F, A]): ValidationT[F, A] =
        ValidationT(
          EitherT(
            fa.a
              .fold(e => f(extruder.core.errorsToThrowable(e)), a => ValidationT[F, A](FFF.pure(a)))
              .flatMap(_.a.value)
          )
        )

      override def ap2[A, B, Z](
        ff: ValidationT[F, (A, B) => Z]
      )(fa: ValidationT[F, A], fb: ValidationT[F, B]): ValidationT[F, Z] = apply.ap2(ff)(fa, fb)

      override def ap[A, B](ff: ValidationT[F, A => B])(fa: ValidationT[F, A]): ValidationT[F, B] =
        apply.ap(ff)(fa)

      protected val apply: Apply[ValidationT[F, ?]] = new Apply[ValidationT[F, ?]] {
        override def ap[A, B](ff: ValidationT[F, A => B])(fa: ValidationT[F, A]): ValidationT[F, B] =
          ValidationT(EitherT(FF.flatMap(ff.a.value) { ff0 =>
            FF.map(fa.a.value) { fa0 =>
              (fa0, ff0) match {
                case (Right(a), Right(f)) => Right(f(a))
                case (Left(e1), Left(e2)) => Left(e2 ++ e1.toList)
                case (Left(e), _) => Left(e)
                case (_, Left(e)) => Left(e)
              }
            }
          }))

        override def map[A, B](fa: ValidationT[F, A])(f: A => B): ValidationT[F, B] = ValidationT(fa.a.map(f))
      }
    }
  }
}
