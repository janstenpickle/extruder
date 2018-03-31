package extruder.metrics.data

import shapeless.{Coproduct, Inl, Inr}

object Numbers {
  def add(l: Numbers, r: Numbers): Numbers = (l, r) match {
    case (Inl(n1), Inl(n2)) => Coproduct[Numbers](n1 + n2)
    case (Inr(Inl(n1)), Inr(Inl(n2))) => Coproduct[Numbers](n1 + n2)
    case (Inr(Inr(Inl(n1))), Inr(Inr(Inl(n2)))) => Coproduct[Numbers](n1 + n2)
    case (Inr(Inr(Inr(Inl(n1)))), Inr(Inr(Inr(Inl(n2))))) => Coproduct[Numbers](n1 + n2)
    case (Inr(Inr(Inr(Inr(Inl(n1))))), Inr(Inr(Inr(Inr(Inl(n2)))))) => Coproduct[Numbers](n1 + n2)
    case (n1, n2) => Coproduct[Numbers](toDouble(n1) + toDouble(n2))
  }

  def to[A](
    fromShort: Short => A,
    fromInt: Int => A,
    fromLong: Long => A,
    fromFloat: Float => A,
    fromDouble: Double => A
  ): Numbers => A = {
    case Inl(s) => fromShort(s)
    case Inr(Inl(i)) => fromInt(i)
    case Inr(Inr(Inl(l))) => fromLong(l)
    case Inr(Inr(Inr(Inl(f)))) => fromFloat(f)
    case Inr(Inr(Inr(Inr(Inl(d))))) => fromDouble(d)
    case Inr(Inr(Inr(Inr(Inr(_))))) => throw new RuntimeException("Impossible!")
  }

  val toDouble: Numbers => Double = to[Double](_.toDouble, _.toDouble, _.toDouble, _.toDouble, identity)
  val toLong: Numbers => Long = to[Long](_.toLong, _.toLong, identity, _.toLong, _.toLong)
}
