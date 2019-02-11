package extruder.core

/**
  * Read a string from input `I` at a path, returning an optional string wrapped in functor `F`.
  *
  * Used with `Parser` or `MultiParser` instances to automatically derive `DecoderT` instances.
  *
  * @tparam F functor to wrap the result in
  * @tparam S settings to use when looking up
  * @tparam I input data to read from
  */
trait StringReader[F[_], S, I] {
  def lookup(path: List[String], settings: S, data: I): F[Option[String]]
}

object StringReader {
  def apply[F[_], S, I](implicit reader: StringReader[F, S, I]): StringReader[F, S, I] = reader
}
