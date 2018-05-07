package extruder.metrics.conversions

import extruder.metrics.conversions.timer._
import extruder.metrics.data.TimerValue
import org.scalacheck.Prop
import org.specs2.matcher.Matchers
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class TimerConversionsSpec extends Specification with ScalaCheck with Matchers {
  override def is: SpecStructure =
    s2"""
        Can convert a long to a timer $testLong
        Can convert a tuple of longs to a completed timer $testTuple
      """

  def testLong: Prop = prop { (l: Long) =>
    val t: TimerValue[Long] = l
    t === TimerValue(l)
  }

  def testTuple: Prop = prop { (ll: (Long, Long)) =>
    val t: TimerValue[Long] = ll
    t === TimerValue(ll._1, Some(ll._2))
  }
}
