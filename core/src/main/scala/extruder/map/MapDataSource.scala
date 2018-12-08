package extruder.map
import extruder.core.{DataSource, Settings}

trait MapDataSource extends DataSource {
  override type Sett = Settings

  override def defaultSettings: Settings = new Settings {
    override def pathToString(path: List[String]): String = path.mkString(".").toLowerCase
  }
}
