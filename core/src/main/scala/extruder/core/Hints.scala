package extruder.core

trait Hints {
  def pathWithType(path: List[String]): List[String] = path :+ TypeKey

  def pathToStringWithType(path: List[String]): String =
    pathToString(pathWithType(path))

  def pathToString(path: List[String]): String

  val includeClassNameInPath: Boolean = true
}

trait HintsCompanion[T <: Hints] {
  implicit def default: T
  def apply(implicit hints: T): T = hints
}
