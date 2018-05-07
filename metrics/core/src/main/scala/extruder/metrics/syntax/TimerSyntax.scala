package extruder.metrics.syntax

import extruder.metrics.data.TimerValue

import scala.concurrent.duration.FiniteDuration

trait TimerSyntax {
  implicit class LongOps(l: Long) {
    def toTimer: TimerValue[Long] = TimerValue(l)
    def toTimer(finish: Long): TimerValue[Long] = TimerValue(l, Some(finish))
    def toTimer(finish: FiniteDuration): TimerValue[Long] = toTimer(finish.toMillis)
  }

  implicit class FiniteDurationOps(dur: FiniteDuration) {
    private def millis = dur.toMillis
    def toTimer: TimerValue[Long] = millis.toTimer
    def toTimer(finish: Long): TimerValue[Long] = millis.toTimer(finish)
    def toTimer(finish: FiniteDuration): TimerValue[Long] = millis.toTimer(finish)
  }

  implicit class LongTimerOps(timer: TimerValue[Long]) {
    def checkpoint(): TimerValue[Long] = timer.checkpoint(System.currentTimeMillis())
  }
}
