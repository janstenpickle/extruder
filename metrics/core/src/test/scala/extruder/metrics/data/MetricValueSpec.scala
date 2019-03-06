package extruder.metrics.data

import cats.Eq
import cats.instances.all._
import cats.kernel.Monoid
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.{Assertion, FunSuite}
import org.typelevel.discipline.scalatest.Discipline

class MetricValueSpec extends FunSuite with Discipline {
  import MetricValueSpec._

  checkAll("Counter monoid", MonoidTests[CounterValue[Int]].monoid)
  checkAll("Gauge monoid", MonoidTests[GaugeValue[Int]].monoid)

  test("Can create a timer value with the start set to the current time")(timerStart)
  test("Timer monoid Can create a timer value with the start set to the current time")(timerMonoidEmpty)
  test("Timer monoid Combines two unfinished timers, setting the start to lowest value")(timerMonoidNotFinished)
  test("Timer monoid Combines two timers where one is finished")(timerMonoidOneFinished)
  test("Timer monoid Combines two timers where both are finished")(timerMonoidBothFinished)

  def timerStart: Assertion = {
    val min = System.currentTimeMillis()
    val timerStart = TimerValue().start
    assert((timerStart >= min) && (timerStart <= System.currentTimeMillis()))
  }

  def timerMonoidEmpty: Assertion = {
    val min = System.currentTimeMillis()
    val timerStart = Monoid[TimerValue[Long]].empty.start
    assert((timerStart >= min) && (timerStart <= System.currentTimeMillis()))
  }

  def timerMonoidNotFinished: Assertion = {
    val first = TimerValue()
    val second = TimerValue()
    assert(Monoid[TimerValue[Long]].combine(first, second).start === first.start)
  }

  def timerMonoidOneFinished: Assertion = {
    val first = TimerValue()
    val second = TimerValue().checkpoint(System.currentTimeMillis())
    val combined = Monoid[TimerValue[Long]].combine(first, second)
    assert((combined.start === first.start) && (combined.finish === second.finish))
  }

  def timerMonoidBothFinished: Assertion = {
    val first = TimerValue()
    val second = TimerValue().checkpoint(System.currentTimeMillis())
    val third = first.checkpoint(System.currentTimeMillis())
    val combined = Monoid[TimerValue[Long]].combine(first, second)
    assert((combined.start === third.start) && (combined.finish === third.finish))
  }
}

object MetricValueSpec {
  implicit def counterEq[A: Eq]: Eq[CounterValue[A]] = Eq.by(_.value)
  implicit def gaugeEq[A: Eq]: Eq[GaugeValue[A]] = Eq.by(_.value)
}
