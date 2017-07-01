package extruder.fs2

import cats.MonadError
import cats.effect.IO
import extruder.core.{ExtruderApplicativeError, Hints, IOConvert, IOF, IOFlatMap}
import fs2.{Strategy, Task}

trait Fs2Instances {
  implicit def taskMonadError(implicit s: Strategy): MonadError[Task, Throwable] = new MonadError[Task, Throwable] {
    override def pure[A](x: A): Task[A] = Task(x)

    override def flatMap[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: (A) => Task[Either[A, B]]): Task[B] = f(a).flatMap {
      case Left(a1) => tailRecM(a1)(f)
      case Right(b) => Task.now(b)
    }

    override def raiseError[A](e: Throwable): Task[A] = Task.fail(e)

    override def handleErrorWith[A](fa: Task[A])(f: (Throwable) => Task[A]): Task[A] = fa.handleWith {
      case th => f(th)
    }
  }

  implicit def taskApplicativeError(
    implicit s: Strategy,
    IOC: IOConvert[Task]
  ): ExtruderApplicativeError[Task, Throwable] =
    new ExtruderApplicativeError.FromMonadError[Task] {
      override def attemptIO[A](a: IO[Task[A]])(implicit hints: Hints): Task[A] =
        IOC.fromIO(a).flatMap(identity)
    }

  implicit def taskIOConvert(implicit s: Strategy): IOConvert[Task] = new IOConvert[Task] {
    override def toIO[A](a: Task[A]): IO[A] = IO.async[A] { cb =>
      a.unsafeRunAsync(cb)
    }

    override def fromIO[A](a: IO[A]): Task[A] = Task.async[A] { cb =>
      a.unsafeRunAsync(cb)
    }
  }

  implicit def taskIOFFlatMap(implicit s: Strategy, IOC: IOConvert[Task]): IOFlatMap[Task] = new IOFlatMap[Task]() {
    override def flatMap[A, B](fa: IOF[Task, A])(f: (A) => IOF[Task, B]): IOF[Task, B] =
      fa.flatMap { tsk =>
        IOC.toIO(tsk).map(f).flatMap(identity)
      }
  }
}
