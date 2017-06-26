package extruder.monix

import cats.Eq
import cats.effect.IO
import extruder.core.IOFlatMapSpec
import extruder.core.TestCommon._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.util.Try

class TaskIOFlatMapSpec extends IOFlatMapSpec[Task, Throwable] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Task[A]] = ioEq[A].on(taskIOConvert.toIO)

  override implicit def IOFEq[A](implicit e: Eq[Task[A]], ea: Eq[A]): Eq[IO[Task[A]]] = new Eq[IO[Task[A]]] {
    override def eqv(a: IO[Task[A]], b: IO[Task[A]]): Boolean = {
      val a1 = Try(taskIOConvert.toIO(a.unsafeRunSync()).unsafeRunSync())
      val b1 = Try(taskIOConvert.toIO(b.unsafeRunSync()).unsafeRunSync())

      tryEq[A].eqv(a1, b1)
    }
  }
}
