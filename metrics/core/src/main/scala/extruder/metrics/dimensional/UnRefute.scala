package extruder.metrics.dimensional

import extruder.core._
import extruder.metrics.data._
import shapeless.ops.hlist.{Length, Mapper, Repeat, Take}
import shapeless.{<:!<, =:!=, Generic, HList, Nat, Poly1, Refute}

import scala.concurrent.duration.FiniteDuration

trait UnRefute[T] {}

object UnRefute {
  implicit def unRefuteGeneric[T, Repr <: HList](
    implicit gen: Generic.Aux[T, Repr],
    mapper: Mapper[genericElements.type, Repr],
    refuteLen: Refute[Length.Aux[Repr, Nat._1]]
  ): UnRefute[T] = new UnRefute[T] {}

  object genericElements extends Poly1 {
    implicit def caseGeneric[A](
      implicit gen: Generic[A],
      ev: A <:!< MetricValue[_],
      ev1: A <:!< ResetNamespace[_],
      ev2: A =:!= FiniteDuration
    ): Case.Aux[A, Unit] = at[A](_ => ())

    implicit def caseNonNumericShow[A](
      implicit show: Show[A],
      refute: Refute[Numeric[A]],
      ev: A =:!= FiniteDuration
    ): Case.Aux[A, Unit] =
      at[A](_ => ())
  }

  implicit def unRefuteIdentical[A, Repr <: HList, Len <: Nat, Taken <: HList, Rep <: HList](
    implicit gen: Generic.Aux[A, Repr],
    len: Length.Aux[Repr, Len],
    take: Take.Aux[Repr, Nat._1, Taken],
    rep: Repeat.Aux[Taken, Len, Rep],
    ev: Rep =:= Repr
  ): UnRefute[A] = new UnRefute[A] {}

  implicit def unRefuteMetricValue[A](implicit ev: A <:< MetricValue[_]): UnRefute[A] = new UnRefute[A] {}
}
