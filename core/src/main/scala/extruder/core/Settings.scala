package extruder.core

import extruder.data.PathElement

trait Settings {
  val typeKey: String = "type"

  final def pathElementListToString(path: List[PathElement]): String =
    pathToString(pathElementsAsStrings(path))

  final def pathElementsAsStrings(path: List[PathElement]): List[String] = path.collect {
    case PathElement.Standard(element) => element
    case PathElement.ClassName(className) if includeClassNameInPath => className
    case PathElement.Type => typeKey
  }

  def pathToString(path: List[String]): String

  val includeClassNameInPath: Boolean = true
}
