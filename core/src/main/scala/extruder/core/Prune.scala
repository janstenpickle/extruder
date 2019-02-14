package extruder.core

import extruder.data.PathElement

/**
  * Used to constrain a data source `I` to a certain path, returning the constrained data source with a copy of
  * all of its keys.
  *
  * Used in some data sources when decoding a map
  *
  * @tparam F Functor in which to wrap the result
  * @tparam S Settings to use when looking up values
  * @tparam I Data to read from and return
  */
trait Prune[F[_], S, I] {
  def prune(path: List[PathElement], settings: S, data: I): F[Option[(List[String], I)]]
}

object Prune {
  def apply[F[_], S, I](implicit prune: Prune[F, S, I]): Prune[F, S, I] = prune
}
