package extruder.monix

import cats.Eq
import extruder.core.ExtruderApplicativeErrorThrowableSpec
import extruder.core.TestCommon._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

class TaskApplicativeErrorSpec extends ExtruderApplicativeErrorThrowableSpec[Task]() {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Task[A]] = ioEq[A].on(taskIOConvert.toIO)
}
