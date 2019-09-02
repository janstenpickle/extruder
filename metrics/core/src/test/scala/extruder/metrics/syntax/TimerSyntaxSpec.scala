package extruder.metrics.syntax

import java.util.concurrent.TimeUnit

import extruder.metrics.data.TimerValue
import extruder.metrics.syntax.timer._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.duration.FiniteDuration

class TimerSyntaxSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  import TimerSyntaxSpec._

  test("Can convert a long into a timer")(testLong)
  test("Can convert two longs into a finished timer")(testLongFinished)
  test("Can convert a long and a FiniteDuration into a finished timer")(testLongFinishedDur)
  test("Can convert a FiniteDuration into a timer")(testDur)
  test("Can convert a FiniteDuration and a long into a finished timer")(testDurFinished)
  test("Can convert two FiniteDurations into a finished timer")(testDurFinishedDur)
  test("Can checkpoint with the current system time")(testCheckpoint)

  def testLong: Assertion = forAll { (l: Long) =>
    assert(l.toTimer === TimerValue(l))
  }

  def testLongFinished: Assertion = forAll { (start: Long, finish: Long) =>
    assert(start.toTimer(finish) === TimerValue(start, Some(finish)))
  }

  def testLongFinishedDur: Assertion = forAll { (start: Long, finish: FiniteDuration) =>
    assert(start.toTimer(finish) === TimerValue(start, Some(finish.toMillis)))
  }

  def testDur: Assertion = forAll { (dur: FiniteDuration) =>
    assert(dur.toTimer === TimerValue(dur.toMillis))
  }

  def testDurFinished: Assertion = forAll { (start: FiniteDuration, finish: Long) =>
    assert(start.toTimer(finish) === TimerValue(start.toMillis, Some(finish)))
  }

  def testDurFinishedDur: Assertion = forAll { (start: FiniteDuration, finish: FiniteDuration) =>
    assert(start.toTimer(finish) === TimerValue(start.toMillis, Some(finish.toMillis)))
  }

  def testCheckpoint: Assertion = forAll { (l: Long) =>
    val t = l.toTimer.checkpoint()
    assert(t === TimerValue(l, Some(t.finish.get)))
  }
}

object TimerSyntaxSpec {
  implicit def durArb: Arbitrary[FiniteDuration] =
    Arbitrary(Gen.posNum[Int].map(i => FiniteDuration(i.toLong, TimeUnit.MILLISECONDS)))
}
