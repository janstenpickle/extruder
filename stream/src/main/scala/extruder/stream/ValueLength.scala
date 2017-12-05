package extruder.stream

import extruder.core.Parser
import shapeless._
import shapeless.ops.nat.{Sum, ToInt}

import scala.annotation.implicitNotFound
@implicitNotFound("Could not create a data source for '${T}'. Currently only case classes and tuples are supported.")
trait ValueLength[T] {
  type Len <: Nat
  val toInt: ToInt[Len]
}

object ValueLength {
  type Aux[T, N <: Nat] = ValueLength[T] {
    type Len = N
  }

  trait DerivedValueLength[T, Repr <: HList, N <: Nat]

  implicit def hNil[T]: DerivedValueLength[T, HNil, Nat._0] = new DerivedValueLength[T, HNil, Nat._0] {}

  implicit def primitive[T: Parser]: ValueLength.Aux[T, Nat._1] = new ValueLength[T] {
    type Len = Nat._1
    override val toInt: ToInt[Len] = ToInt[Len]
  }

  implicit def optional[T: Parser]: ValueLength.Aux[Option[T], Nat._1] = new ValueLength[Option[T]] {
    type Len = Nat._1
    override val toInt: ToInt[Len] = ToInt[Len]
  }

  implicit def hCons[T, H, TailRepr <: HList, HLen <: Nat, TLen <: Nat, OutLen <: Nat](
    implicit h: Lazy[ValueLength.Aux[H, HLen]],
    t: DerivedValueLength[T, TailRepr, TLen],
    sum: Lazy[Sum.Aux[HLen, TLen, OutLen]]
  ): DerivedValueLength[T, H :: TailRepr, OutLen] =
    new DerivedValueLength[T, H :: TailRepr, OutLen] {}

  implicit def product[T, GenRepr <: HList, N <: Nat](
    implicit gen: Generic.Aux[T, GenRepr],
    p: DerivedValueLength[T, GenRepr, N],
    ti: ToInt[N]
  ): ValueLength.Aux[T, N] =
    new ValueLength[T] {
      type Len = N
      override val toInt: ToInt[Len] = ti
    }

  def apply[T](implicit vl: ValueLength[T]): ValueLength[T] = vl
}
