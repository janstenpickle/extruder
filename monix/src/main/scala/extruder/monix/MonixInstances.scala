package extruder.monix

import cats.effect.IO
import extruder.core.{ExtruderApplicativeError, Hints, IOConvert, IOF, IOFlatMap}
import monix.cats._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}

import scala.util.{Failure, Success}

trait MonixInstances {
  implicit def taskApplicativeError(
    implicit scheduler: Scheduler,
    IOC: IOConvert[Task]
  ): ExtruderApplicativeError[Task, Throwable] =
    new ExtruderApplicativeError.FromMonadError[Task]() {
      override def attemptIO[A](a: IO[Task[A]])(implicit utils: Hints): Task[A] =
        IOC.fromIO(a).flatten
    }

  implicit def taskIOConvert(implicit scheduler: Scheduler): IOConvert[Task] = new IOConvert[Task] {
    override def toIO[A](a: Task[A]): IO[A] = IO.async[A] { cb =>
      a.runOnComplete { t =>
        cb(t match {
          case Success(a1) => Right(a1)
          case Failure(th) => Left(th)
        })
      }
    }

    override def fromIO[A](a: IO[A]): Task[A] = Task.async[A] { (_, cb) =>
      a.unsafeRunAsync { x =>
        cb.apply(x match {
          case Right(a1) => Success(a1)
          case Left(th) => Failure(th)
        })
      }
      Cancelable.empty
    }
  }

  implicit def taskIOFFlatMap(implicit scheduler: Scheduler, IOC: IOConvert[Task]): IOFlatMap[Task] =
    new IOFlatMap[Task]() {
      override def flatMap[A, B](fa: IOF[Task, A])(f: (A) => IOF[Task, B]): IOF[Task, B] =
        fa.flatMap(tsk => IOC.toIO(tsk).map(f).flatMap(identity))
    }
}
