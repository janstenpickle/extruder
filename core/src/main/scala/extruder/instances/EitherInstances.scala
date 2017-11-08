package extruder.instances

import cats.MonadError
import cats.data.EitherT
import cats.effect.{Async, IO}
import cats.instances.either._
import extruder.core.{EitherErrors, EitherThrowable, ExtruderApplicativeError, Hints, IOFlatMap, ValidationErrors}

trait EitherInstances {
  import ExtruderApplicativeError._

//  implicit val throwableExtruderApplicativeError: ExtruderApplicativeError[EitherThrowable, Throwable] =
//    new FromMonadError[EitherThrowable] {
//      override def ap[A, B](ff: Either[Throwable, (A) => B])(fa: Either[Throwable, A]): Either[Throwable, B] =
//        appendThrowables(ff, fa)
//    }

  implicit val throwableExtruderApplicativeError: ExtruderApplicativeError[EitherThrowable, Throwable] =
    new WithMonadError[EitherThrowable, Throwable] {
//      override def ap[A, B](ff: Either[Throwable, (A) => B])(fa: Either[Throwable, A]): Either[Throwable, B] =
//        appendThrowables(ff, fa)
      override def ME = cats.instances.either.catsStdInstancesForEither

      override def missing[A](message: String) = ME.raiseError(new NoSuchElementException(message))

      override def validationFailure[A](message: String) = ME.raiseError(new RuntimeException(message))

      override def validationException[A](message: String, ex: Throwable) = ME.raiseError(ex)

      override def flatMap[A, B](fa: EitherThrowable[A])(f: (A) => EitherThrowable[B]) = ME.flatMap(fa)(f)
    }

//  implicit val validationErrorsExtruderApplicativeError: ExtruderApplicativeError[EitherErrors, ValidationErrors] =
//    new FromMonadErrorAccumulatesErrors[EitherErrors] {
//      override def ap[A, B](
//        ff: Either[ValidationErrors, (A) => B]
//      )(fa: Either[ValidationErrors, A]): Either[ValidationErrors, B] =
//        (ff, fa) match {
//          case (Right(f), Right(a)) => Right(f(a))
//          case (Left(fe), Left(ae)) => Left(fe ++ ae.toList)
//          case (Left(e), Right(_)) => Left(e)
//          case (Right(_), Left(e)) => Left(e)
//        }
//    }

  implicit def ioFlatMapForEither[T]: IOFlatMap[Either[T, ?]] = new IOFlatMap[Either[T, ?]] {
    override def flatMap[A, B](fa: IO[Either[T, A]])(f: (A) => IO[Either[T, B]]): IO[Either[T, B]] =
      fa.flatMap {
        case Right(v) => f(v)
        case Left(e) => IO(Left(e))
      }
  }
}
