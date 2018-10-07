package extruder.core

import cats.{Apply, MonadError}
import cats.instances.all._
import cats.data.NonEmptyList

object Validation {

  implicit def eitherErrorsMonadErrors: MonadError[Validation, Throwable] =
    new MonadError[Validation, Throwable] {
      def F: MonadError[Validation, ValidationErrors] =
        MonadError[Validation, ValidationErrors]

      override def raiseError[A](e: Throwable): Validation[A] =
        Left(NonEmptyList.of(ValidationException(e.getMessage, e)))

      override def handleErrorWith[A](fa: Validation[A])(f: (Throwable) => Validation[A]): Validation[A] =
        fa.fold(e => f(errorsToThrowable(e)), Right(_))

      override def flatMap[A, B](fa: Validation[A])(f: (A) => Validation[B]): Validation[B] =
        F.flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: (A) => Validation[Either[A, B]]): Validation[B] =
        F.tailRecM(a)(f)

      override def pure[A](x: A): Validation[A] = F.pure(x)

      override def ap2[A, B, Z](ff: Validation[(A, B) => Z])(fa: Validation[A], fb: Validation[B]): Validation[Z] =
        eitherApply.ap2(ff)(fa, fb)

      override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validation[B] =
        eitherApply.ap(ff)(fa)

    }

  protected val eitherApply: Apply[Validation] = new Apply[Validation] {
    override def ap[A, B](ff: Validation[A => B])(fa: Validation[A]): Validation[B] =
      (fa, ff) match {
        case (Right(a), Right(f)) => Right(f(a))
        case (Left(e1), Left(e2)) => Left(e2 ++ e1.toList)
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
      }

    override def map[A, B](fa: Validation[A])(f: A => B): Validation[B] = fa.map(f)
  }
}
