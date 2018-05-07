package extruder.metrics.syntax

import java.util.concurrent.TimeUnit

import extruder.metrics.data.TimerValue
import extruder.metrics.syntax.timer._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.matcher.Matchers
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

import scala.concurrent.duration.FiniteDuration

class TimerSyntaxSpec extends Specification with ScalaCheck with Matchers {
  import TimerSyntaxSpec._

  override def is: SpecStructure =
    s2"""
        Can convert a long into a timer $testLong
        Can convert two longs into a finished timer $testLongFinished
        Can convert a long and a FiniteDuration into a finished timer $testLongFinishedDur
        Can convert a FiniteDuration into a timer $testDur
        Can convert a FiniteDuration and a long into a finished timer $testDurFinished
        Can convert two FiniteDurations into a finished timer $testDurFinishedDur

        Can checkpoint with the current system time $testCheckpoint
      """

  def testLong: Prop = prop { (l: Long) =>
    l.toTimer === TimerValue(l)
  }

  def testLongFinished: Prop = prop { (start: Long, finish: Long) =>
    start.toTimer(finish) === TimerValue(start, Some(finish))
  }

  def testLongFinishedDur: Prop = prop { (start: Long, finish: FiniteDuration) =>
    start.toTimer(finish) === TimerValue(start, Some(finish.toMillis))
  }

  def testDur: Prop = prop { (dur: FiniteDuration) =>
    dur.toTimer === TimerValue(dur.toMillis)
  }

  def testDurFinished: Prop = prop { (start: FiniteDuration, finish: Long) =>
    start.toTimer(finish) === TimerValue(start.toMillis, Some(finish))
  }

  def testDurFinishedDur: Prop = prop { (start: FiniteDuration, finish: FiniteDuration) =>
    start.toTimer(finish) === TimerValue(start.toMillis, Some(finish.toMillis))
  }

  def testCheckpoint: Prop = prop { (l: Long) =>
    val t = l.toTimer.checkpoint()
    t === TimerValue(l, Some(t.finish.get))
  }
}

object TimerSyntaxSpec {
  implicit def durArb: Arbitrary[FiniteDuration] =
    Arbitrary(Gen.posNum[Int].map(i => FiniteDuration(i.toLong, TimeUnit.MILLISECONDS)))
}
