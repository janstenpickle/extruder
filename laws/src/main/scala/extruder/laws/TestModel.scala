package extruder.laws

import cats.Eq

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
