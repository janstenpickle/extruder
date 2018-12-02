package extruder.core

trait DataSource {
  type InputData
  type OutputData
  type Sett <: Settings

  def defaultSettings: Sett
}
