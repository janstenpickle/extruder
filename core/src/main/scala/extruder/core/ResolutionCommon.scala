package extruder.core

import cats.syntax.validated._

trait ResolutionCommon extends Serializable {
  val TypeKey = "type"

  val ListSeparator: String = ","

  protected def pathWithType(path: Seq[String]): Seq[String] = path :+ TypeKey

  protected def pathToStringWithType(path: Seq[String]): String =
    pathToString(pathWithType(path))

  protected def pathToString(path: Seq[String]): String

  protected def errorMsg[T](path: Seq[String]): ConfigValidation[T] =
    Missing(s"Could not find configuration at '${pathToString(path)}' and no default available").invalidNel[T]
}
