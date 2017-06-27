package extruder.core

import java.net.URL

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
case class AnotherCaseClass(url: URL, d: Sealed)
