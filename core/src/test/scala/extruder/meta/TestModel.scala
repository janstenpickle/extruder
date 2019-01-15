package extruder.meta

import java.net.URL

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}

sealed trait Sealed
case class CCImpl(d: Double) extends Sealed
case object ObjImpl extends Sealed

case class CaseClass(
  s: String,
  i: Int,
  l: Long,
  opt: Option[Int],
  map: Map[String, Long],
  either: Either[Int, String],
  collection: List[Int],
  chain: Chain[Int],
  nel: NonEmptyList[Int],
  nec: NonEmptyChain[Int],
  nev: NonEmptyVector[Int],
  an: Option[AnotherCaseClass],
  default: Double = 1.0,
  defaultOption: Option[Boolean] = None
)
case class AnotherCaseClass(url: URL, d: Sealed)
