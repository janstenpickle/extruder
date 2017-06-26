package extruder.core

import scala.concurrent.duration._

trait Hints {
  val ListSeparator: String = ","

  def pathWithType(path: Seq[String]): Seq[String] = path :+ TypeKey

  def pathToStringWithType(path: Seq[String]): String =
    pathToString(pathWithType(path))

  def pathToString(path: Seq[String]): String

  def ioTimeout: Duration = 3.seconds
}

trait HintsCompanion[T <: Hints] {
  implicit def default: T
  def apply(implicit utils: T): T = utils
}
