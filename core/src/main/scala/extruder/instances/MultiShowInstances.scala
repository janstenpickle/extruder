package extruder.instances

import cats.Contravariant
import extruder.core.MultiShow

trait MultiShowInstances {
  implicit val extruderStdInstancesForMultiShow: Contravariant[MultiShow] = new Contravariant[MultiShow] {
    override def contramap[A, B](fa: MultiShow[A])(f: B => A): MultiShow[B] = fa.contramap(f)
  }
}
