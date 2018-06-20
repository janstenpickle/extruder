package extruder.metrics.data

import cats.Monoid
import cats.syntax.monoid._

case class MetricValues[T[A] <: MetricValue[A], K, V](values: Map[K, T[V]]) extends AnyVal {
  def +(elem: (K, T[V]))(implicit num: Numeric[V], V: Monoid[T[V]]): MetricValues[T, K, V] = {
    val (k, v) = elem
    MetricValues(values + (k -> (v |+| values.getOrElse(k, V.empty))))
  }
}

object MetricValues {
  implicit def monoid[T[A] <: MetricValue[A], K, V: Numeric](implicit B: Monoid[T[V]]): Monoid[MetricValues[T, K, V]] =
    new Monoid[MetricValues[T, K, V]] {
      override def empty: MetricValues[T, K, V] = MetricValues[T, K, V](Map.empty)
      override def combine(x: MetricValues[T, K, V], y: MetricValues[T, K, V]): MetricValues[T, K, V] =
        y.values.foldLeft(x)(_ + _)
    }
}
