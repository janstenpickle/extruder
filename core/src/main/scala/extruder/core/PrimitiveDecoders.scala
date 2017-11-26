package extruder.core

import java.net.URL

import cats.Applicative
import cats.implicits._
import mouse.string._
import shapeless.syntax.typeable._
import shapeless.{Lazy, LowPriority, Typeable}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

trait PrimitiveDecoders extends { self: Decoders with DecodeTypes =>
  protected def hasValue[F[_]](path: List[String], data: DecodeData)(implicit hints: Hint, F: Eff[F]): F[Boolean]

  protected def lookupValue[F[_]](path: List[String], data: DecodeData)(
    implicit hints: Hint,
    F: Eff[F]
  ): F[Option[String]]

  implicit def primitiveDecoder[F[_], T](
    implicit parser: Parser[T],
    hints: Hint,
    F: Eff[F],
    lp: LowPriority
  ): Dec[F, T] =
    mkDecoder[F, T]((path, default, data) => resolveValue(formatParserError(parser, path)).apply(path, default, data))

  implicit def optionalDecoder[F[_], T](implicit decoder: Lazy[Dec[F, T]], hints: Hint, F: Eff[F]): Dec[F, Option[T]] =
    mkDecoder[F, Option[T]] { (path, _, data) =>
      val decoded = decoder.value.read(path, None, data)
      val lookedUp = hasValue[F](path, data)

      val evaluateOptional: (Either[Throwable, T], Boolean) => F[Option[T]] = {
        case (Right(v), _) => F.pure(Some(v))
        case (Left(_), true) => F.map(decoded)(Some(_))
        case (_, false) => F.pure(None)
      }

      F.flatMap(F.attempt(decoded))(att => F.flatMap(lookedUp)(lu => evaluateOptional(att, lu)))
    }

  protected def resolveValue[F[_], E, T](
    parser: String => F[T]
  )(implicit hints: Hint, AE: Eff[F]): (List[String], Option[T], DecodeData) => F[T] =
    resolve[F, T, String](parser, lookupValue[F])

  protected def resolve[F[_], T, V](
    parser: V => F[T],
    lookup: (List[String], DecodeData) => F[Option[V]]
  )(path: List[String], default: Option[T], data: DecodeData)(implicit hints: Hint, F: Eff[F]): F[T] =
    F.flatMap(lookup(path, data)) { v =>
      (v, default) match {
        case (None, None) =>
          F.missing(s"Could not find value at '${hints.pathToString(path)}' and no default available")
        case (None, Some(value)) => F.pure(value)
        case (Some(value), _) => parser(value)
      }
    }

  protected def formatParserError[F[_], T](
    parser: Parser[T],
    path: List[String]
  )(implicit hints: Hint, F: Eff[F]): String => F[T] =
    value =>
      parser
        .parse(value)
        .fold[F[T]](
          err => F.validationFailure(s"Could not parse value '$value' at '${hints.pathToString(path)}': $err"),
          F.pure
      )
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
  implicit def duration[T <: Duration: Typeable: ClassTag](implicit ct: ClassTag[T]): Parser[T] =
    Parser(
      dur =>
        Either
          .catchNonFatal(Duration(dur))
          .leftMap(_.getMessage)
          .flatMap(
            a =>
              Either.fromOption(
                a.cast[T],
                s"Could not parse value '$dur' as a valid duration for type '${ct.runtimeClass.getSimpleName}'"
            )
        )
    )

  def traversableBuilder[T, F[T] <: TraversableOnce[T]](
    split: String => List[String]
  )(implicit parser: Parser[T], cbf: CanBuildFrom[List[T], T, F[T]]): Parser[F[T]] =
    Parser(
      input =>
        Applicative[Either[String, ?]]
          .sequence(split(input).filterNot(_.isEmpty).map(parser.parse))
          .map(_.map[T, F[T]](identity))
    )

  implicit def traversable[T, F[T] <: TraversableOnce[T]](
    implicit parser: Parser[T],
    cbf: CanBuildFrom[List[T], T, F[T]]
  ): Parser[F[T]] =
    traversableBuilder[T, F](_.split(',').toList)
}

case class Parser[T](parse: String => Either[String, T])

object Parser extends Parsers {
  def apply[T](implicit parser: Parser[T]): Parser[T] = parser

  def fromEitherException[T](parse: String => Either[Throwable, T]): Parser[T] =
    Parser[T](parse.andThen(_.leftMap(_.getMessage)))
}
