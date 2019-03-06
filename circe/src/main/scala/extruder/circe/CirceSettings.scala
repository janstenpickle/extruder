package extruder.circe

import extruder.core.Settings

trait CirceSettings extends Settings {
  override val includeClassNameInPath: Boolean = false
  def formatElement(element: String): String
  override def pathToString(path: List[String]): String = path.map(formatElement).mkString(".")
}
