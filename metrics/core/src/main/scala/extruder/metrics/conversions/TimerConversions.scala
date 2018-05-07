package extruder.metrics.conversions

import extruder.metrics.data.TimerValue

trait TimerConversions {
  implicit def longToTimer(l: Long): TimerValue[Long] = TimerValue(l)
  implicit def tupleToTimer(ll: (Long, Long)): TimerValue[Long] = TimerValue(ll._1, Some(ll._2))
}
