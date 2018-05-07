package extruder.metrics.dimensional

import cats.kernel.Monoid

case class ResetNamespace[T](value: T) extends AnyVal

object ResetNamespace {
  implicit def monoid[A](implicit A: Monoid[A]): Monoid[ResetNamespace[A]] = new Monoid[ResetNamespace[A]] {
    override def empty: ResetNamespace[A] = ResetNamespace[A](A.empty)

    override def combine(x: ResetNamespace[A], y: ResetNamespace[A]): ResetNamespace[A] =
      ResetNamespace[A](A.combine(x.value, y.value))
  }
}
