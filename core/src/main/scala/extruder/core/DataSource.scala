package extruder.core

import extruder.effect.ExtruderMonadError

trait DataSource {
  type InputData
  type OutputData
  type Eff[F[_]] <: ExtruderMonadError[F]
  type Sett <: Settings

  def defaultSettings: Sett
}
