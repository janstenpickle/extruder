package extruder.circe

import extruder.core.DataSource

trait CirceDataSource extends DataSource {
  override type Sett = CirceSettings

  override val defaultSettings: CirceSettings = new CirceSettings {
    override def formatElement(element: String): String = element
  }
}
