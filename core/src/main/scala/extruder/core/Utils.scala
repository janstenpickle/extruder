package extruder.core

import cats.syntax.validated._

trait Utils {
  val ListSeparator: String = ","

  def pathWithType(path: Seq[String]): Seq[String] = path :+ TypeKey

  def pathToStringWithType(path: Seq[String]): String =
    pathToString(pathWithType(path))

  def pathToString(path: Seq[String]): String

  def errorMsg[T](path: Seq[String]): ConfigValidation[T] =
    Missing(s"Could not find configuration at '${pathToString(path)}' and no default available").invalidNel[T]
}

trait UtilsCompanion[T <: Utils] {
  implicit def default: T
  def apply(implicit configuration: T): T = configuration
}

trait UtilsMixin {
  type U <: Utils
  def utils: U
}
