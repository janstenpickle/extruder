package extruder.core

trait Settings {
  def pathWithType(path: List[String]): List[String] = path :+ TypeKey

  def pathToStringWithType(path: List[String]): String =
    pathToString(pathWithType(path))

  def pathToString(path: List[String]): String

  val includeClassNameInPath: Boolean = true
}
