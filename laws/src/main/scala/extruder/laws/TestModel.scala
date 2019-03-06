package extruder.laws

import cats.Eq
import cats.instances.tuple._
import org.scalacheck.Arbitrary

sealed trait Sealed
case class CCImpl(d: Double) extends Sealed
case object ObjImpl extends Sealed

case class CaseClass(
  s: String,
  i: Int,
  l: Long,
  an: Option[AnotherCaseClass],
  default: Double = 1.0,
  defaultOption: Option[Boolean] = None
)

object CaseClass {
  implicit val eq: Eq[CaseClass] = Eq.fromUniversalEquals
}

case class AnotherCaseClass(str: String, d: Sealed)

class MultiClass[A, B](val a: A, val b: B)

object MultiClass {
  implicit def eq[A: Eq, B: Eq]: Eq[MultiClass[A, B]] = Eq.by(m => (m.a, m.b))
  implicit def arb[A, B](implicit arbA: Arbitrary[A], arbB: Arbitrary[B]): Arbitrary[MultiClass[A, B]] =
    Arbitrary(for {
      a <- arbA.arbitrary
      b <- arbB.arbitrary
    } yield new MultiClass[A, B](a, b))
}
