package extruder.core

import cats.effect.IO
import extruder.instances.{FutureInstances, IOInstances}

trait IOConvert[M[_]] {
  def toIO[A](a: M[A]): IO[A]
  def fromIO[A](a: IO[A]): M[A]
}

object IOConvert extends FutureInstances with IOInstances {
  def apply[M[_]](IOC: IOConvert[M]): IOConvert[M] = IOC
}