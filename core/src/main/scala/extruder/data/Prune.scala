package extruder.data

trait Prune[F[_], S, D] {
  def prune(path: List[String], settings: S, data: D): F[Option[(List[String], D)]]
}

object Prune {
  def apply[F[_], S, D](implicit prune: Prune[F, S, D]): Prune[F, S, D] = prune
}
