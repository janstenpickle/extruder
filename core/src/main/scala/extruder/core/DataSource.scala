package extruder.core

import cats.MonadError

trait DataSource {
  type InputData
  type OutputData
  type Eff[F[_]] <: MonadError[F, Throwable]
  type Sett <: Settings

  def defaultSettings: Sett
}
