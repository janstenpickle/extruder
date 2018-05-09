package extruder.instances

import cats.Functor
import extruder.core.MultiParser

trait MultiParserInstances {
  implicit def extruderStdInstancesForMultiParser[F[_]: Functor]: Functor[MultiParser[F, ?]] =
    new Functor[MultiParser[F, ?]] {
      override def map[A, B](fa: MultiParser[F, A])(f: A => B): MultiParser[F, B] = fa.map(f)
    }
}
