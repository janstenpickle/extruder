package shapelessconfig.core

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.either._
import cats.syntax.validated._
import mouse.string._
import shapelessconfig.syntax.validation.ConfigValidation

trait Resolvers extends Serializable {
  def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]]

  def pathToString(path: Seq[String]): String

  def errorMsg[T](path: Seq[String]): ConfigValidation[T] =
    s"Could not find configuration at '${pathToString(path)}' and no default available".invalidNel[T]

  def resolveEither[T](parser: String => Either[Throwable, T])
                      (path: Seq[String], default: Option[T]): ConfigValidation[T] =
    resolve(parser.andThen(
      _.leftMap(ex => s"Could not parse value for '${pathToString(path)}': ${ex.getMessage}").toValidatedNel
    ))(path, default)

  def resolve[T](parser: String => ConfigValidation[T])(path: Seq[String], default: Option[T]): ConfigValidation[T] =
    (resolveConfig(path), default) match {
      case (Valid(None), None) => errorMsg[T](path)
      case (Valid(None), Some(v)) => v.validNel[String]
      case (Valid(Some(str)), _) => parser(str)
      case (err @ Invalid(_), _) => err
    }

  implicit val string: Resolver[String] = Resolver(resolve(_.validNel))
  implicit val int: Resolver[Int] = Resolver(resolveEither(_.parseInt))
  implicit val long: Resolver[Long] = Resolver(resolveEither(_.parseLong))
  implicit val double: Resolver[Double] = Resolver(resolveEither(_.parseDouble))
  implicit val float: Resolver[Float] = Resolver(resolveEither(_.parseFloat))
  implicit val short: Resolver[Short] = Resolver(resolveEither(_.parseShort))
  implicit val byte: Resolver[Byte] = Resolver(resolveEither(_.parseByte))
  implicit val boolean: Resolver[Boolean] = Resolver(resolveEither(_.parseBoolean))

  implicit def optional[T](implicit resolver: Resolver[T]): Resolver[Option[T]] =
    Resolver((path, default) =>
      (resolveConfig(path), default) match {
        case (Valid(None), None) => None.validNel
        case (Valid(None), Some(d)) => d.validNel
        case (Valid(Some(_)), _) => resolver.read(path, None).map(Some(_))
        case (err @ Invalid(_), _) => err
      }
    )

  implicit def optionalCaseClass[T <: Product with Serializable](implicit resolver: Resolver[T]): Resolver[Option[T]] =
    Resolver((path, _) => resolver.read(path, None).map(Some(_)))
}

trait Resolver[T] {
  def read(path: Seq[String], default: Option[T]): ConfigValidation[T]
}

object Resolver {
  def apply[T](f: (Seq[String], Option[T]) => ConfigValidation[T]) = new Resolver[T] {
    override def read(path: Seq[String],
                      default: Option[T]): ConfigValidation[T] = f(path, default)
  }
}