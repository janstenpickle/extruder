package extruder.metrics.data

import cats.Monoid
import cats.syntax.monoid._

case class MetricValues[T[A] <: MetricValue[A], V](values: Map[String, T[V]]) extends AnyVal {
  def +(elem: (String, T[V]))(implicit num: Numeric[V], V: Monoid[T[V]]): MetricValues[T, V] = {
    val (k, v) = elem
    MetricValues(values + (k -> (v |+| values.getOrElse(k, V.empty))))
  }
}

object MetricValues {
  implicit def monoid[A[T] <: MetricValue[T], B: Numeric](implicit B: Monoid[A[B]]): Monoid[MetricValues[A, B]] =
    new Monoid[MetricValues[A, B]] {
      override def empty: MetricValues[A, B] = MetricValues[A, B](Map.empty)
      override def combine(x: MetricValues[A, B], y: MetricValues[A, B]): MetricValues[A, B] =
        y.values.foldLeft(x)(_ + _)
    }
}
