package extruder.metrics.data

import cats.Monoid
import extruder.metrics.data.MetricType.{Counter, Gauge, Timer}

import scala.math.Numeric.Implicits._

trait MetricValue[T] extends Any {
  def value: T
  def metricType: MetricType
}

case class GaugeValue[T](value: T) extends AnyVal with MetricValue[T] {
  override def metricType: MetricType = Gauge
  def +(rhs: GaugeValue[T])(implicit num: Numeric[T]): GaugeValue[T] = GaugeValue(value + rhs.value)
}

object GaugeValue {
  implicit def monoid[A](implicit num: Numeric[A]): Monoid[GaugeValue[A]] = new Monoid[GaugeValue[A]] {
    override def empty: GaugeValue[A] = GaugeValue(num.zero)
    override def combine(x: GaugeValue[A], y: GaugeValue[A]): GaugeValue[A] = x + y
  }
}

case class CounterValue[T](value: T) extends AnyVal with MetricValue[T] {
  override def metricType: MetricType = Counter
  def +(rhs: CounterValue[T])(implicit num: Numeric[T]): CounterValue[T] = CounterValue(value + rhs.value)
}

object CounterValue {
  implicit def monoid[A](implicit num: Numeric[A]): Monoid[CounterValue[A]] = new Monoid[CounterValue[A]] {
    override def empty: CounterValue[A] = CounterValue(num.zero)
    override def combine(x: CounterValue[A], y: CounterValue[A]): CounterValue[A] = x + y
  }
}

case class TimerValue[T](start: T, finish: Option[T] = None)(implicit num: Numeric[T]) extends MetricValue[T] {
  override lazy val value: T = finish.fold(num.zero)(_ - start)
  override val metricType: MetricType = Timer

  def checkpoint(time: T): TimerValue[T] = copy(finish = Some(time))
  def isFinished: Boolean = finish.isDefined
}

object TimerValue {
  def apply(): TimerValue[Long] = TimerValue(System.currentTimeMillis())

  // Note this is not a lawful monoid as the empty value is the current time
  implicit val monoid: Monoid[TimerValue[Long]] = new Monoid[TimerValue[Long]] {
    override def empty: TimerValue[Long] = TimerValue(System.currentTimeMillis())

    override def combine(x: TimerValue[Long], y: TimerValue[Long]): TimerValue[Long] = {
      val finish =
        if (x.isFinished || y.isFinished) Some(math.max(x.finish.getOrElse(0L), y.finish.getOrElse(0L)))
        else None

      TimerValue(math.min(x.start, y.start), finish)
    }
  }
}
