package extruder.core
import cats.Applicative

trait OptionSelector {
  protected def selectOption[F[_], A, S <: Settings](
    path: List[String],
    settings: S,
    primary: Option[A],
    secondary: Option[A]
  )(implicit F: Applicative[F], error: ExtruderErrors[F]): F[A] =
    (primary, secondary) match {
      case (None, None) =>
        error.missing(s"Could not find value at '${settings.pathToString(path)}' and no default available")
      case (None, Some(value)) => F.pure(value)
      case (Some(value), _) => F.pure(value)
    }
}
