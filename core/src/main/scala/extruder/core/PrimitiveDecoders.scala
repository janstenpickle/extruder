package extruder.core

import java.net.URL

import cats.effect.IO
import cats.implicits._
import mouse.string._
import shapeless.syntax.typeable._
import shapeless.{Lazy, Typeable}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

trait PrimitiveDecoders { self: Decoders with DecodeTypes =>
  protected def hasValue[F[_], E](path: List[String], data: DecodeData)(
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): IO[F[Boolean]]
  protected def lookupValue[F[_], E](path: List[String], data: DecodeData)(
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): IO[F[Option[String]]]
  protected def lookupList[F[_], E](
    path: List[String],
    data: DecodeData
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[List[String]]]] =
    lookupValue[F, E](path, data).map(_.map(_.map(_.split(hints.ListSeparator).toList.map(_.trim))))

  implicit def primitiveDecoder[F[_], E, T](
    implicit parser: Parser[T],
    hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): Dec[F, T] =
    mkDecoder[F, T]((path, default, data) => resolveValue(formatParserError(parser, path)).apply(path, default, data))

  implicit def traversableDecoder[F[_], E, T, FF[T] <: TraversableOnce[T]](
    implicit parser: Parser[T],
    cbf: CanBuildFrom[FF[T], T, FF[T]],
    hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): Dec[F, FF[T]] =
    mkDecoder { (path, default, data) =>
      resolveList { x =>
        val seq: F[List[T]] = AE.sequence(x.filterNot(_.isEmpty).map(formatParserError(parser, path)))

        AE.map(seq)(_.foldLeft(cbf())(_ += _).result())
      }.apply(path, default, data)
    }

  implicit def optionalDecoder[F[_], E, T](
    implicit decoder: Lazy[Dec[F, T]],
    hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): Dec[F, Option[T]] =
    mkDecoder[F, Option[T]] { (path, _, data) =>
      val evaluateOptional: (Either[E, T], Boolean) => F[Option[T]] = {
        case (Right(v), _) => AE.pure(Some(v))
        case (Left(e), true) => AE.raiseError(e)
        case (_, false) => AE.pure(None)
      }

      for {
        attempted <- decoder.value.read(path, None, data).map(AE.attempt)
        lookedUp <- hasValue[F, E](path, data)
      } yield AE.flatMap(attempted)(att => AE.flatMap(lookedUp)(lu => evaluateOptional(att, lu)))
    }

  protected def resolveValue[F[_], E, T](
    parser: String => F[T]
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): (List[String], Option[T], DecodeData) => IO[F[T]] =
    resolve[F, E, T, String](parser, lookupValue[F, E])

  protected def resolveList[F[_], E, T](
    parser: List[String] => F[T]
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): (List[String], Option[T], DecodeData) => IO[F[T]] =
    resolve[F, E, T, List[String]](parser, lookupList[F, E])

  protected def resolve[F[_], E, T, V](parser: V => F[T], lookup: (List[String], DecodeData) => IO[F[Option[V]]])(
    path: List[String],
    default: Option[T],
    data: DecodeData
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[T]] = {

    lookup(path, data).map(
      AE.flatMap[Option[V], T](_)(
        v =>
          (v, default) match {
            case (None, None) =>
              AE.missing(s"Could not find datauration at '${hints.pathToString(path)}' and no default available")
            case (None, Some(value)) => AE.pure(value)
            case (Some(value), _) => parser(value)
        }
      )
    )
  }

  protected def formatParserError[F[_], E, T](
    parser: Parser[T],
    path: List[String]
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): String => F[T] =
    value =>
      parser
        .parse(value)
        .fold[F[T]](
          err => AE.validationFailure(s"Could not parse value '$value' at '${hints.pathToString(path)}': $err"),
          AE.pure
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
}

case class Parser[T](parse: String => Either[String, T])

object Parser extends Parsers {
  def apply[T](implicit parser: Parser[T]): Parser[T] = parser

  def fromEitherException[T](parse: String => Either[Throwable, T]): Parser[T] =
    Parser[T](parse.andThen(_.leftMap(_.getMessage)))
}
