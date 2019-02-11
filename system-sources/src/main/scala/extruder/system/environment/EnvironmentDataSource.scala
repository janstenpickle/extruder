package extruder.system.environment
import extruder.core.{DataSource, Settings}

trait EnvironmentDataSource extends DataSource {
  override type Sett = Settings

  override def defaultSettings: Settings = new Settings {
    override def pathToString(path: List[String]): String = path.mkString("_").toUpperCase
  }
}
