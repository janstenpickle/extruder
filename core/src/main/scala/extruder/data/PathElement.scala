package extruder.data

sealed trait PathElement extends Any

object PathElement {
  case class Standard(value: String) extends AnyVal with PathElement
  case class ClassName(value: String) extends AnyVal with PathElement
  case object Type extends PathElement
}
