package extruder.core

import scala.concurrent.duration._

trait Hints {
  val ListSeparator: String = ","

  def pathWithType(path: List[String]): List[String] = path :+ TypeKey

  def pathToStringWithType(path: List[String]): String =
    pathToString(pathWithType(path))

  def pathToString(path: List[String]): String

  def prependtoPath(prepend: String, path: String): String

  def ioTimeout: Duration = 3.seconds
}

trait HintsCompanion[T <: Hints] {
  implicit def default: T
  def apply(implicit hints: T): T = hints
}
