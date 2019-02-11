package extruder.core

/**
  * Write a string to output data `O` wrapped in functor `F`.
  *
  * @tparam F
  * @tparam S
  * @tparam O
  */
trait StringWriter[F[_], S, O] {
  def write(path: List[String], settings: S, value: String): F[O]
}

object StringWriter {
  def apply[F[_], S, O](implicit writer: StringWriter[F, S, O]): StringWriter[F, S, O] = writer
}
