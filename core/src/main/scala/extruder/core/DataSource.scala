package extruder.core

trait DataSource {
  type InputData
  type OutputData
  type Hint <: Hints
}
