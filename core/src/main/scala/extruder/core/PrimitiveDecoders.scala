package extruder.core

import java.net.URL

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import mouse.string._
import shapeless.syntax.typeable._
import shapeless.{Lazy, Typeable}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

trait PrimitiveDecoders[C, D[T] <: Decoder[T, C]] { self: Decoders[C, D] with UtilsMixin =>
  protected def lookupValue(path: Seq[String], config: C): ConfigValidation[Option[String]]
  protected def lookupList(path: Seq[String], config: C): ConfigValidation[Option[List[String]]] =
    lookupValue(path, config).map(_.map(_.split(utils.ListSeparator).toList.map(_.trim)))

  implicit def primitiveDecoder[T](implicit parser: Parser[T]): D[T] =
    mkDecoder((path, default, config) => resolveValue(formatParserError(parser, path))(path, default, config))

  implicit def traversableDecoder[T, F[T] <: TraversableOnce[T]](implicit parser: Parser[T],
                                                                 cbf: CanBuildFrom[F[T], T, F[T]]): D[F[T]] =
    mkDecoder((path, default, config) =>
      resolveList(_.filterNot(_.isEmpty).
        map(formatParserError(parser, path)).
        sequenceU.
        map(_.foldLeft(cbf())(_ += _).result())
      )(path, default, config)
    )

  implicit def optionalDecoder[T](implicit decoder: Lazy[D[T]]): D[Option[T]] =
    mkDecoder((path, _, config) => decoder.value.read(path, None, config).fold(x =>
      if (x.filter(_.isInstanceOf[Missing]) == x.toList) None.validNel
      else x.invalid,
      Some(_).validNel)
    )

  protected def resolveValue[T](parser: String => ConfigValidation[T]): (Seq[String], Option[T], C) => ConfigValidation[T] =
    resolve[T, String](parser, lookupValue)

  protected def resolveList[T](parser: List[String] => ConfigValidation[T]): (Seq[String], Option[T], C) => ConfigValidation[T] =
    resolve[T, List[String]](parser, lookupList)

  protected def resolve[T, V](parser: V => ConfigValidation[T],
                    lookup: (Seq[String], C) => ConfigValidation[Option[V]])(path: Seq[String], default: Option[T], config: C): ConfigValidation[T] =
    (lookup(path, config), default) match {
      case (Valid(None), None) => utils.errorMsg[T](path)
      case (Valid(None), Some(v)) => v.validNel[ValidationError]
      case (Valid(Some(v)), _) => parser(v)
      case (err @ Invalid(_), _) => err
    }

  protected def formatParserError[T](parser: Parser[T], path: Seq[String]): String => ConfigValidation[T] = value =>
    parser.parse(value).leftMap(err =>
       ValidationFailure(s"Could not parse value '$value' at '${utils.pathToString(path)}': $err")
    ).toValidatedNel
}

trait Parsers {
  implicit val char: Parser[Char] = Parser(_.toCharArray.toList match {
    case x :: Nil => Right(x)
    case _: Any => Left(s"Not a valid Char")
  })
  implicit val string: Parser[String] = Parser(Right(_))
  implicit val int: Parser[Int] = Parser.fromEitherException(_.parseInt)
  implicit val long: Parser[Long] = Parser.fromEitherException(_.parseLong)
  implicit val double: Parser[Double] = Parser.fromEitherException(_.parseDouble)
  implicit val float: Parser[Float] = Parser.fromEitherException(_.parseFloat)
  implicit val short: Parser[Short] = Parser.fromEitherException(_.parseShort)
  implicit val byte: Parser[Byte] = Parser.fromEitherException(_.parseByte)
  implicit val boolean: Parser[Boolean] = Parser.fromEitherException(_.parseBoolean)
  implicit val url: Parser[URL] = Parser.fromEitherException(url => Either.catchNonFatal(new URL(url)))
  implicit def duration[T <: Duration : Typeable : ClassTag](implicit ct: ClassTag[T]): Parser[T] = Parser(dur =>
    Either.catchNonFatal(Duration(dur)).leftMap(_.getMessage).flatMap(a => Either.fromOption(
      a.cast[T],
      s"Could not parse value '$dur' as a valid duration for type '${ct.runtimeClass.getSimpleName}'"
    ))
  )
}

case class Parser[T](parse: String => Either[String, T])

object Parser extends Parsers {
  def apply[T](implicit parser: Parser[T]): Parser[T] = parser

  def fromEitherException[T](parse: String => Either[Throwable, T]): Parser[T] =
    Parser[T](parse.andThen(_.leftMap(_.getMessage)))
}
