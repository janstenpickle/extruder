package extruder.core

trait DataSource {
  type Sett <: Settings

  def defaultSettings: Sett
}
