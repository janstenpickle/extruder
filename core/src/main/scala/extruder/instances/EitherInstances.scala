package extruder.instances

import cats.effect.IO
import cats.instances.either._
import extruder.core.{EitherErrors, EitherThrowable, ExtruderApplicativeError, IOFlatMap, Hints, ValidationErrors}

trait EitherInstances {
  import ExtruderApplicativeError._

  implicit val throwableExtruderApplicativeError: ExtruderApplicativeError[EitherThrowable, Throwable] =
    new FromMonadError[EitherThrowable] {
      override def ap[A, B](ff: Either[Throwable, (A) => B])
                           (fa: Either[Throwable, A]): Either[Throwable, B] =
        appendThrowables(ff, fa)
    }

  implicit val validationErrorsExtruderApplicativeError: ExtruderApplicativeError[EitherErrors, ValidationErrors] =
    new FromMonadErrorAccumulatesErrors[EitherErrors] {
      override def ap[A, B](ff: Either[ValidationErrors, (A) => B])
                           (fa: Either[ValidationErrors, A]): Either[ValidationErrors, B] =
        (ff, fa) match {
          case (Right(f), Right(a)) => Right(f(a))
          case (Left(fe), Left(ae)) => Left(fe ++ ae.toList)
          case (Left(e), Right(_)) => Left(e)
          case (Right(_), Left(e)) => Left(e)
        }
    }

  implicit def ioFlatMapForEither[T]: IOFlatMap[Either[T, ?]] = new IOFlatMap[Either[T, ?]] {
    override def flatMap[A, B](fa: IO[Either[T, A]])(f: (A) => IO[Either[T, B]]): IO[Either[T, B]] =
      fa.flatMap {
        case Right(v) => f(v)
        case Left(e) => IO(Left(e))
      }
  }
}
