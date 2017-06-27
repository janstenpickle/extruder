package extruder.instances

import cats.Eval
import cats.effect.IO
import cats.instances.future._
import extruder.core.{ExtruderApplicativeError, Hints, IOConvert, IOF, IOFlatMap}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait FutureInstances {
  import ExtruderApplicativeError._

  implicit def extruderApplicativeErrorForFuture(
    implicit ec: ExecutionContext,
    IOC: IOConvert[Future]
  ): ExtruderApplicativeError[Future, Throwable] = new FromMonadError[Future] {
    override def attemptIO[A](a: IO[Future[A]])(implicit utils: Hints): Future[A] = IOC.fromIO(a).flatMap(identity)

    override def ap[A, B](ff: Future[(A) => B])(fa: Future[A]): Future[B] = {
      def tryToEither[T]: Try[T] => Either[Throwable, T] = {
        case Success(a) => Right(a)
        case Failure(err) => Left(err)
      }

      ff.onComplete(ffTry => fa.onComplete(faTry => appendThrowables(tryToEither(ffTry), tryToEither(faTry))))

      super.ap(ff)(fa)
    }
  }

  implicit def ioConvertForFuture(implicit ec: ExecutionContext): IOConvert[Future] = new IOConvert[Future] {
    override def fromIO[A](a: IO[A]): Future[A] = a.unsafeToFuture()
    override def toIO[A](a: Future[A]): IO[A] = IO.fromFuture(Eval.later(a))
  }

  implicit def ioFlatMapForFuture(implicit ec: ExecutionContext, IOC: IOConvert[Future]): IOFlatMap[Future] =
    new IOFlatMap[Future]() {
      override def flatMap[A, B](fa: IOF[Future, A])(f: (A) => IOF[Future, B]): IOF[Future, B] =
        fa.flatMap(fut => IOC.toIO(fut).map(f).flatMap(identity))
    }
}
