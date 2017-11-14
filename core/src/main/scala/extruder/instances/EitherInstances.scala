package extruder.instances

import cats.Monad
import extruder.core.{Missing, ValidationError, ValidationException, ValidationFailure}
import extruder.effect.ExtruderMonadError
import cats.syntax.either._
import cats.instances.all._

trait EitherInstances {
  val eitherMonadError: ExtruderMonadError[Either[ValidationError, ?]] =
    new ExtruderMonadError[Either[ValidationError, ?]] {
      private def F: Monad[Either[ValidationError, ?]] = Monad[Either[ValidationError, ?]]

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
}
