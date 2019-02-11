package extruder.typesafe

import extruder.core.{DataSource, Settings}

trait TypesafeConfigDataSource extends DataSource {
  override type Sett = Settings

  override def defaultSettings: Settings = new Settings {
    val kebabCaseTransformation: String => String =
      _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1-$2").replaceAll("([a-z\\d])([A-Z])", "$1-$2").toLowerCase

    override def pathToString(path: List[String]): String = path.map(kebabCaseTransformation).mkString(".")
    override val includeClassNameInPath: Boolean = false
  }
}
