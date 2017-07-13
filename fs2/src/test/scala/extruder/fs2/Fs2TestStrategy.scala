package extruder.fs2

import fs2.Strategy

import scala.concurrent.ExecutionContext

object Fs2TestStrategy {
  implicit val strategy: Strategy = Strategy.fromExecutionContext(ExecutionContext.global)
}
