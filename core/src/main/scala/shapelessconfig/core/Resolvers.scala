package shapelessconfig.core

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import mouse.string._
import shapelessconfig.syntax.validation.ConfigValidation

import scala.collection.generic.CanBuildFrom

trait Resolvers extends Serializable {
  import Resolvers._

  def lookupValue(path: Seq[String]): ConfigValidation[Option[String]]
  def pathToString(path: Seq[String]): String
  def lookupList(path: Seq[String]): ConfigValidation[Option[List[String]]] =
    lookupValue(path).map(_.map(_.split(",").toList.map(_.trim)))

  implicit val string: Parser[String] = Right(_)
  implicit val int: Parser[Int] = _.parseInt
  implicit val long: Parser[Long] = _.parseLong
  implicit val double: Parser[Double] = _.parseDouble
  implicit val float: Parser[Float] = _.parseFloat
  implicit val short: Parser[Short] = _.parseShort
  implicit val byte: Parser[Byte] = _.parseByte
  implicit val boolean: Parser[Boolean] = _.parseBoolean

  implicit def parseType[T](implicit parse: Parser[T]): Resolver[T] =
    Resolver((path, default) => resolveValue(formatParserError(parse, path))(path, default))

  implicit def parseTraversable[T, F[T] <: TraversableOnce[T]](implicit parser: Parser[T],
                                                               cbf: CanBuildFrom[F[T], T, F[T]]): Resolver[F[T]] =
    Resolver((path, default) =>
      resolveList(_.filterNot(_.isEmpty).
        map(formatParserError(parser, path)).
        sequenceU.
        map(_.foldLeft(cbf())(_ += _).result())
      )(path, default)
    )

  implicit def optional[T](implicit resolver: Resolver[T]): Resolver[Option[T]] =
    Resolver((path, default) =>
      (lookupValue(path), default) match {
        case (Valid(None), None) => None.validNel
        case (Valid(None), Some(d)) => d.validNel
        case (Valid(Some(_)), _) => resolver.read(path, None).map(Some(_))
        case (err @ Invalid(_), _) => err
      }
    )

  implicit def optionalCaseClass[T <: Product with Serializable](implicit resolver: Resolver[T]): Resolver[Option[T]] =
    Resolver((path, _) => resolver.read(path, None).map(Some(_)))

  def errorMsg[T](path: Seq[String]): ConfigValidation[T] =
    ValidationFailure(s"Could not find configuration at '${pathToString(path)}' and no default available")

  def resolveValue[T](parser: String => ConfigValidation[T]): (Seq[String], Option[T]) => ConfigValidation[T] =
    resolve[T, String](parser, lookupValue)

  def resolveList[T](parser: List[String] => ConfigValidation[T]): (Seq[String], Option[T]) => ConfigValidation[T] =
    resolve[T, List[String]](parser, lookupList)

  def resolve[T, V](parser: V => ConfigValidation[T],
                    lookup: Seq[String] => ConfigValidation[Option[V]])(path: Seq[String], default: Option[T]): ConfigValidation[T] =
    (lookup(path), default) match {
      case (Valid(None), None) => errorMsg[T](path)
      case (Valid(None), Some(v)) => v.validNel[ValidationFailure]
      case (Valid(Some(v)), _) => parser(v)
      case (err @ Invalid(_), _) => err
    }

  def formatParserError[T](parser: Parser[T], path: Seq[String]): String => ConfigValidation[T] = value =>
    parser(value).leftMap(ex =>
       new ValidationFailure(s"Could not parse value '$value' at '${pathToString(path)}': ${ex.getClass.getName}", Some(ex))
    ).toValidatedNel
}

object Resolvers {
  type Parser[T] = String => Either[Throwable, T]
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