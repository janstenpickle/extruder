package extruder.data

trait StringReader[F[_], S, I] {
  def lookup(path: List[String], settings: S, data: I): F[Option[String]]
}

object StringReader {
  def apply[F[_], S, O](implicit reader: StringReader[F, S, O]): StringReader[F, S, O] = reader
}
