package extruder.instances

import cats.Contravariant
import extruder.core.Show

trait ShowInstances {
  implicit val extruderStdInstancesForShow: Contravariant[Show] = new Contravariant[Show] {
    override def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.make[B]((fa.show _).compose(f))
  }
}
