package extruder.monix

import cats.effect.{Async, Effect, IO}
import monix.eval.{Callback, Task}
import monix.execution.Scheduler

import scala.util.{Failure, Success, Try}

trait MonixInstances {
  implicit def taskEffect(implicit scheduler: Scheduler): Effect[Task] = new Effect[Task] {
    private val noop = (_: Either[Throwable, Any]) => ()

    override def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] = Task.tailRecM(a)(f)

    override def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.flatMap(f)

    override def raiseError[A](e: Throwable): Task[A] = Task.raiseError(e)

    override def handleErrorWith[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] = fa.onErrorHandleWith(f)

    override def pure[A](x: A): Task[A] = Task.now(x)

    override def delay[A](thunk: => A): Task[A] =
      Task.eval(thunk)
    override def suspend[A](fa: => Task[A]): Task[A] =
      Task.defer(fa)
    override def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): Task[A] =
      Task.unsafeCreate { (_, cb) =>
        k { r =>
          val r0 = r.fold[Try[A]](Failure(_), Success(_))
          cb(r0)
        }
      }

    override def runAsync[A](fa: Task[A])(cb: Either[Throwable, A] => IO[Unit]) =
      IO(fa.runAsync(new Callback[A] {
        def onSuccess(value: A): Unit =
          cb(Right(value)).unsafeRunAsync(noop)
        def onError(ex: Throwable): Unit =
          cb(Left(ex)).unsafeRunAsync(noop)
      }))
  }
}
