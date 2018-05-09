package extruder.metrics.data

import cats.kernel.Monoid
import shapeless.{Coproduct, Inl, Inr}

object Numbers {
  def add(l: Numbers, r: Numbers): Numbers = (l, r) match {
    case (Inl(n1), Inl(n2)) => Coproduct[Numbers](n1 + n2) // short plus short
    case (Inl(n1), Inr(Inl(n2))) => Coproduct[Numbers](n1.toInt + n2) // short plus int
    case (Inl(n1), Inr(Inr(Inl(n2)))) => Coproduct[Numbers](n1.toLong + n2) // short plus long
    case (Inl(n1), Inr(Inr(Inr(Inl(n2))))) => Coproduct[Numbers](n1.toFloat + n2) // short plus float
    case (Inl(n1), Inr(Inr(Inr(Inr(Inl(n2)))))) => Coproduct[Numbers](n1.toDouble + n2) // short plus double
    case (Inr(Inl(n1)), Inr(Inl(n2))) => Coproduct[Numbers](n1 + n2) // int plus int
    case (Inr(Inl(n1)), Inl(n2)) => Coproduct[Numbers](n1 + n2.toInt) // int plus short
    case (Inr(Inl(n1)), Inr(Inr(Inl(n2)))) => Coproduct[Numbers](n1.toLong + n2) // int plus long
    case (Inr(Inl(n1)), Inr(Inr(Inr(Inl(n2))))) => Coproduct[Numbers](n1.toFloat + n2) // int plus float
    case (Inr(Inl(n1)), Inr(Inr(Inr(Inr(Inl(n2)))))) => Coproduct[Numbers](n1.toDouble + n2) // int plus double
    case (Inr(Inr(Inl(n1))), Inr(Inr(Inl(n2)))) => Coproduct[Numbers](n1 + n2) // long plus long
    case (Inr(Inr(Inl(n1))), Inl(n2)) => Coproduct[Numbers](n1 + n2.toLong) // long plus short
    case (Inr(Inr(Inl(n1))), Inr(Inl(n2))) => Coproduct[Numbers](n1 + n2.toLong) // long plus int
    case (Inr(Inr(Inl(n1))), Inr(Inr(Inr(Inl(n2))))) => Coproduct[Numbers](n1.toDouble + n2.toDouble) // long plus float
    case (Inr(Inr(Inl(n1))), Inr(Inr(Inr(Inr(Inl(n2)))))) => Coproduct[Numbers](n1.toDouble + n2) // long plus double
    case (Inr(Inr(Inr(Inl(n1)))), Inr(Inr(Inr(Inl(n2))))) => Coproduct[Numbers](n1 + n2) // float plus float
    case (Inr(Inr(Inr(Inl(n1)))), Inl(n2)) => Coproduct[Numbers](n1 + n2.toFloat) // float plus short
    case (Inr(Inr(Inr(Inl(n1)))), Inr(Inl(n2))) => Coproduct[Numbers](n1 + n2.toFloat) // float plus int
    case (Inr(Inr(Inr(Inl(n1)))), Inr(Inr(Inl(n2)))) => Coproduct[Numbers](n1.toDouble + n2.toDouble) // float plus long
    case (Inr(Inr(Inr(Inl(n1)))), Inr(Inr(Inr(Inr(Inl(n2)))))) =>
      Coproduct[Numbers](n1.toDouble + n2) // float plus double
    case (Inr(Inr(Inr(Inr(Inl(n1))))), Inr(Inr(Inr(Inr(Inl(n2)))))) => Coproduct[Numbers](n1 + n2) // double plus double
    case (Inr(Inr(Inr(Inr(Inl(n1))))), Inl(n2)) => Coproduct[Numbers](n1 + n2.toDouble) // double plus short
    case (Inr(Inr(Inr(Inr(Inl(n1))))), Inr(Inl(n2))) => Coproduct[Numbers](n1 + n2.toDouble) // double plus int
    case (Inr(Inr(Inr(Inr(Inl(n1))))), Inr(Inr(Inl(n2)))) => Coproduct[Numbers](n1 + n2.toDouble) // double plus long
    case (Inr(Inr(Inr(Inr(Inl(n1))))), Inr(Inr(Inr(Inl(n2))))) =>
      Coproduct[Numbers](n1 + n2.toDouble) // double plus float
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
