package extruder.instances

import cats.instances.all._
import extruder.core.ExtruderEffect

trait EitherInstances {
  val eitherEffect: ExtruderEffect[Either[Throwable, ?]] = new ExtruderEffect.FromMonadError[Either[Throwable, ?]]()
}
