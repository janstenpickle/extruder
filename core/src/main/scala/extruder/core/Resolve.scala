package extruder.core
import cats.{Applicative, Monad, Traverse}
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._

trait Resolve extends OptionSelector {
  protected def resolveValue[F[_], S <: Settings, T, D](parser: String => F[T])(
    implicit F: Monad[F],
    error: ExtruderErrors[F],
    reader: StringReader[F, S, D]
  ): (List[String], S, Option[T], D) => F[T] =
    resolve[F, T, String, S, D](parser, reader.lookup)

  protected def resolve[F[_], T, V, S <: Settings, D](parser: V => F[T], lookup: (List[String], S, D) => F[Option[V]])(
    path: List[String],
    settings: S,
    default: Option[T],
    data: D
  )(implicit F: Monad[F], error: ExtruderErrors[F]): F[T] =
    for {
      v <- lookup(path, settings, data)
      parsed <- Traverse[Option].sequence(v.map(parser))
      result <- selectOption[F, T, S](path, settings, parsed, default)
    } yield result

  protected def formatParserError[F[_], T, S <: Settings](parser: Parser[T], path: List[String], settings: S)(
    implicit F: Applicative[F],
    error: ExtruderErrors[F]
  ): String => F[T] =
    value =>
      parser
        .parse(value)
        .fold[F[T]](
          err => error.validationFailure(s"Could not parse value '$value' at '${settings.pathToString(path)}': $err"),
          F.pure
      )

}
