package extruder.metrics.conversions

import extruder.metrics.conversions.timer._
import extruder.metrics.data.TimerValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TimerConversionsSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  test("Can convert a long to a timer")(forAll { l: Long =>
    val t: TimerValue[Long] = l
    t === TimerValue(l)
  })

  test(" Can convert a tuple of longs to a completed timer")(forAll { ll: (Long, Long) =>
    val t: TimerValue[Long] = ll
    t === TimerValue(ll._1, Some(ll._2))
  })
}
