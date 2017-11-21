package extruder.instances

import cats.data.{EitherT, NonEmptyList}
import cats.instances.all._
import cats.syntax.either._
import cats.{Apply, Eval, Monad, MonadError}
import extruder.core
import extruder.core._
import extruder.effect.{ExtruderMonadError, ExtruderSync}

import scala.util.control.NonFatal

trait EitherInstances {
  implicit val eitherMonadError: ExtruderMonadError[Either[ValidationError, ?]] =
    new ExtruderMonadError[Either[ValidationError, ?]] {
      private def F: Monad[Either[ValidationError, ?]] = catsStdInstancesForEither

      override def validationException[A](message: String, ex: Throwable): Either[ValidationError, A] =
        Left(ValidationException(message, ex))

      override def missing[A](message: String): Either[ValidationError, A] = Left(Missing(message))

      override def validationFailure[A](message: String): Either[ValidationError, A] = Left(ValidationFailure(message))

      override def raiseError[A](e: Throwable): Either[ValidationError, A] = validationException(e.getMessage, e)

      override def handleErrorWith[A](
        fa: Either[ValidationError, A]
      )(f: Throwable => Either[ValidationError, A]): Either[ValidationError, A] =
        fa.leftMap[Throwable] {
            case Missing(message) => new NoSuchElementException(message)
            case ValidationFailure(message) => new RuntimeException(message)
            case ValidationException(_, e) => e
          }
          .fold(f, pure)

      override def pure[A](x: A): Either[ValidationError, A] = F.pure(x)

      override def tailRecM[A, B](a: A)(f: A => Either[ValidationError, Either[A, B]]): Either[ValidationError, B] =
        F.tailRecM(a)(f)

      override def flatMap[A, B](fa: Either[ValidationError, A])(
        f: A => Either[ValidationError, B]
      ): Either[ValidationError, B] = F.flatMap(fa)(f)
    }

  implicit def eitherErrorsMonadErrors: ExtruderMonadError[Either[ValidationErrors, ?]] =
    new ExtruderMonadError[Either[ValidationErrors, ?]] {
      def F: MonadError[Either[ValidationErrors, ?], ValidationErrors] =
        MonadError[Either[ValidationErrors, ?], ValidationErrors]

      override def missing[A](message: String): Either[ValidationErrors, A] = Left(NonEmptyList.of(Missing(message)))

      override def validationFailure[A](message: String): Either[ValidationErrors, A] =
        Left(NonEmptyList.of(ValidationFailure(message)))

      override def validationException[A](message: String, ex: Throwable): Either[ValidationErrors, A] =
        Left(NonEmptyList.of(ValidationException(message, ex)))

      override def raiseError[A](e: Throwable): Either[ValidationErrors, A] = validationException(e.getMessage, e)

      override def handleErrorWith[A](
        fa: Either[ValidationErrors, A]
      )(f: (Throwable) => Either[ValidationErrors, A]): Either[ValidationErrors, A] =
        fa.fold(e => f(core.errorsToThrowable(e)), Right(_))

      override def flatMap[A, B](
        fa: Either[ValidationErrors, A]
      )(f: (A) => Either[ValidationErrors, B]): Either[ValidationErrors, B] =
        F.flatMap(fa)(f)

      override def tailRecM[A, B](
        a: A
      )(f: (A) => Either[ValidationErrors, Either[A, B]]): Either[ValidationErrors, B] =
        F.tailRecM(a)(f)

      override def pure[A](x: A): Either[ValidationErrors, A] = F.pure(x)

      override def ap2[A, B, Z](
        ff: Either[ValidationErrors, (A, B) => Z]
      )(fa: Either[ValidationErrors, A], fb: Either[ValidationErrors, B]): Either[ValidationErrors, Z] =
        eitherApply.ap2(ff)(fa, fb)

      override def ap[A, B](
        ff: Either[ValidationErrors, (A) => B]
      )(fa: Either[ValidationErrors, A]): Either[ValidationErrors, B] =
        eitherApply.ap(ff)(fa)
    }

  implicit val eitherTEvalSync: ExtruderSync[EitherT[Eval, ValidationErrors, ?]] =
    new ExtruderSync[EitherT[Eval, ValidationErrors, ?]] {
      def F: MonadError[EitherT[Eval, ValidationErrors, ?], ValidationErrors] =
        MonadError[EitherT[Eval, ValidationErrors, ?], ValidationErrors]

      override def suspend[A](thunk: => EitherT[Eval, ValidationErrors, A]): EitherT[Eval, ValidationErrors, A] =
        EitherT[Eval, ValidationErrors, A](Eval.always(try {
          thunk.value.value
        } catch {
          case NonFatal(t) => Left(NonEmptyList.of(ValidationException(t.getMessage, t)))
        }))

      override def validationException[A](message: String, ex: Throwable): EitherT[Eval, ValidationErrors, A] =
        EitherT.leftT[Eval, A](NonEmptyList.of(ValidationException(message, ex)))

      override def missing[A](message: String): EitherT[Eval, ValidationErrors, A] =
        EitherT.leftT[Eval, A](NonEmptyList.of(Missing(message)))

      override def validationFailure[A](message: String): EitherT[Eval, ValidationErrors, A] =
        EitherT.leftT[Eval, A](NonEmptyList.of(ValidationFailure(message)))

      override def raiseError[A](e: Throwable): EitherT[Eval, ValidationErrors, A] =
        validationException(e.getMessage, e)

      override def handleErrorWith[A](
        fa: EitherT[Eval, ValidationErrors, A]
      )(f: Throwable => EitherT[Eval, ValidationErrors, A]): EitherT[Eval, ValidationErrors, A] =
        EitherT(
          fa.value.flatMap(_.fold(errs => f.andThen(_.value)(core.errorsToThrowable(errs)), a => Eval.now(Right(a))))
        )

      override def pure[A](x: A): EitherT[Eval, ValidationErrors, A] = F.pure(x)

      override def tailRecM[A, B](a: A)(
        f: A => EitherT[Eval, ValidationErrors, Either[A, B]]
      ): EitherT[Eval, ValidationErrors, B] = F.tailRecM(a)(f)

      override def flatMap[A, B](
        fa: EitherT[Eval, ValidationErrors, A]
      )(f: A => EitherT[Eval, ValidationErrors, B]): EitherT[Eval, ValidationErrors, B] =
        F.flatMap(fa)(f)

      override def ap[A, B](
        ff: EitherT[Eval, ValidationErrors, A => B]
      )(fa: EitherT[Eval, ValidationErrors, A]): EitherT[Eval, ValidationErrors, B] =
        EitherT(for {
          fa0 <- fa.value
          ff0 <- ff.value
        } yield eitherApply.ap(ff0)(fa0))

      override def ap2[A, B, Z](ff: EitherT[Eval, ValidationErrors, (A, B) => Z])(
        fa: EitherT[Eval, ValidationErrors, A],
        fb: EitherT[Eval, ValidationErrors, B]
      ): EitherT[Eval, ValidationErrors, Z] =
        EitherT(for {
          fa0 <- fa.value
          fb0 <- fb.value
          ff0 <- ff.value
        } yield eitherApply.ap2(ff0)(fa0, fb0))
    }

  protected val eitherApply: Apply[Either[ValidationErrors, ?]] = new Apply[Either[ValidationErrors, ?]] {
    override def ap[A, B](
      ff: Either[ValidationErrors, A => B]
    )(fa: Either[ValidationErrors, A]): Either[NonEmptyList[ValidationError], B] =
      (fa, ff) match {
        case (Right(a), Right(f)) => Right(f(a))
        case (Left(e1), Left(e2)) => Left(e2 ++ e1.toList)
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
      }

    override def map[A, B](fa: Either[ValidationErrors, A])(f: A => B): Either[ValidationErrors, B] = fa.map(f)
  }
}
