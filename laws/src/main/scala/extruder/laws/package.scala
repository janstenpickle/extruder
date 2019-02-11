package extruder

import cats.Eq
import cats.data.NonEmptyList
import cats.laws.IsEq
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.util.Pretty

package object laws {
  implicit def catsLawsIsEqToProp[A: Eq](isEq: IsEq[A])(implicit pp: A => Pretty): Prop =
    cats.kernel.laws.discipline.catsLawsIsEqToProp[A](isEq)

  implicit def nonEmptyListArb[T](implicit arb: Arbitrary[T]): Arbitrary[NonEmptyList[T]] =
    Arbitrary(for {
      head <- arb.arbitrary
      tail <- Gen.listOf(arb.arbitrary)
    } yield NonEmptyList.of(head, tail: _*))
}
