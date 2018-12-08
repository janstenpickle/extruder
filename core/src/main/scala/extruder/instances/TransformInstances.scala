package extruder.instances
import cats.Functor
import extruder.data.Transform

trait TransformInstances {
  implicit def extruderStdInstancesForPrepare[F[_]: Functor, S, I]: Functor[Transform[F, S, I, ?]] =
    new Functor[Transform[F, S, I, ?]] {
      override def map[A, B](fa: Transform[F, S, I, A])(f: A => B): Transform[F, S, I, B] = fa.map(f)
    }
}
