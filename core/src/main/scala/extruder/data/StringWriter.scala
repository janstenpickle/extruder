package extruder.data

trait StringWriter[F[_], S, O] {
  def write(path: List[String], settings: S, value: String): F[O]
}

object StringWriter {
  def apply[F[_], S, O](implicit writer: StringWriter[F, S, O]): StringWriter[F, S, O] = writer
}
