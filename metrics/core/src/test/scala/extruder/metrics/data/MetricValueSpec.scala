package extruder.metrics.data

import cats.Eq
import cats.instances.all._
import cats.kernel.Monoid
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.ScalacheckShapeless._
import org.specs2.execute.Result
import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class MetricValueSpec extends Specification with Discipline {
  import MetricValueSpec._

  override def is: SpecStructure =
    s2"""
      ${checkAll("Counter monoid", MonoidTests[CounterValue[Int]].monoid)}
      ${checkAll("Gauge monoid", MonoidTests[GaugeValue[Int]].monoid)}

      Can create a timer value with the start set to the current time $timerStart

      Timer monoid
        Can create a timer value with the start set to the current time $timerMonoidEmpty
        Combines two unfinished timers, setting the start to lowest value $timerMonoidNotFinished
        Combines two timers where one is finished $timerMonoidOneFinished
        Combines two timers where both are finished $timerMonoidBothFinished
      """

  def timerStart: Result = {
    val min = System.currentTimeMillis()
    val timerStart = TimerValue().start
    (timerStart >= min).and(timerStart <= System.currentTimeMillis())
  }

  def timerMonoidEmpty: Result = {
    val min = System.currentTimeMillis()
    val timerStart = Monoid[TimerValue[Long]].empty.start
    (timerStart >= min).and(timerStart <= System.currentTimeMillis())
  }

  def timerMonoidNotFinished: Result = {
    val first = TimerValue()
    val second = TimerValue()
    Monoid[TimerValue[Long]].combine(first, second).start === first.start
  }

  def timerMonoidOneFinished: Result = {
    val first = TimerValue()
    val second = TimerValue().checkpoint(System.currentTimeMillis())
    val combined = Monoid[TimerValue[Long]].combine(first, second)
    (combined.start === first.start).and(combined.finish === second.finish)
  }

  def timerMonoidBothFinished: Result = {
    val first = TimerValue()
    val second = TimerValue().checkpoint(System.currentTimeMillis())
    val third = first.checkpoint(System.currentTimeMillis())
    val combined = Monoid[TimerValue[Long]].combine(first, second)
    (combined.start === third.start).and(combined.finish === third.finish)
  }
}

object MetricValueSpec {
  implicit def counterEq[A: Eq]: Eq[CounterValue[A]] = Eq.by(_.value)
  implicit def gaugeEq[A: Eq]: Eq[GaugeValue[A]] = Eq.by(_.value)
}
