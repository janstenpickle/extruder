package extruder.fs2

import cats.Eq
import extruder.core.ExtruderApplicativeErrorThrowableSpec
import extruder.core.TestCommon._
import extruder.fs2.Fs2TestStrategy._
import fs2.Task

class TaskApplicativeErrorSpec extends ExtruderApplicativeErrorThrowableSpec[Task]() {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Task[A]] = ioEq[A].on(taskIOConvert.toIO)
}
