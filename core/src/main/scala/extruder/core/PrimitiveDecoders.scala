package extruder.core

import java.net.URL

import cats.{Monad, Traverse}
import cats.data.{NonEmptyList, OptionT, ValidatedNel}
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import mouse.string._
import shapeless.syntax.typeable._
import shapeless.{Lazy, LowPriority, Refute, Typeable}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

trait PrimitiveDecoders { self: Decoders with DecodeTypes =>
  protected def hasValue[F[_]](path: List[String], data: DecodeData)(implicit hints: Hint, F: Eff[F]): F[Boolean]

  protected def lookupValue[F[_]](path: List[String], data: DecodeData)(
    implicit hints: Hint,
    F: Eff[F]
  ): F[Option[String]]

  implicit def parserDecoder[F[_], T](implicit parser: Parser[T], hints: Hint, F: Eff[F], lp: LowPriority): Dec[F, T] =
    mkDecoder[F, T]((path, default, data) => resolveValue(formatParserError(parser, path)).apply(path, default, data))

  implicit def optionalMultiParserDecoder[F[_], T](
    implicit parser: MultiParser[T],
    hints: Hint,
    F: Eff[F]
  ): Dec[F, Option[T]] =
    mkDecoder[F, Option[T]]((path, _, data) => multiParse(parser, path, data))

  implicit def multiParserDecoder[F[_], T](
    implicit parser: MultiParser[T],
    hints: Hint,
    F: Eff[F],
    lp: LowPriority
  ): Dec[F, T] =
    mkDecoder[F, T](
      (path, default, data) =>
        for {
          parsed <- multiParse(parser, path, data)
          result <- selectOption(path, parsed, default)
        } yield result
    )

  private def multiParse[F[_], T](parser: MultiParser[T], path: List[String], data: DecodeData)(
    implicit F: Eff[F],
    hints: Hint
  ): F[Option[T]] =
    parser
      .parse(path, p => OptionT(lookupValue[F](p, data)))
      .value
      .flatMap(
        v =>
          Traverse[Option]
            .sequence[ValidatedNel[String, ?], T](v)
            .fold[F[Option[T]]](
              errs =>
                errs.tail
                  .foldLeft(F.validationFailure[Option[T]](errs.head))(
                    (acc, v) => F.ap2[Option[T], Option[T], Option[T]](F.pure((l, _) => l))(acc, F.validationFailure(v))
                ),
              F.pure
          )
      )

  implicit def nonEmptyListDecoder[F[_], T](
    implicit decoder: Lazy[Dec[F, List[T]]],
    F: Eff[F],
    hints: Hint
  ): Dec[F, NonEmptyList[T]] = mkDecoder[F, NonEmptyList[T]] { (path, default, data) =>
    val decoded = decoder.value.read(path, default.map(_.toList), data)
    decoded
      .map(NonEmptyList.fromList)
      .flatMap[NonEmptyList[T]](
        _.fold[F[NonEmptyList[T]]](
          F.validationFailure(
            s"List at '${hints.pathToString(path)}' must contain data, but is empty, and no default available"
          )
        )(F.pure)
      )
  }

  implicit def optionalDecoder[F[_], T](
    implicit decoder: Lazy[Dec[F, T]],
    hints: Hint,
    F: Eff[F],
    refute: Refute[MultiParser[T]]
  ): Dec[F, Option[T]] =
    mkDecoder[F, Option[T]] { (path, _, data) =>
      val decoded = decoder.value.read(path, None, data)
      val lookedUp = hasValue[F](path, data)

      val evaluateOptional: (Either[Throwable, T], Boolean) => F[Option[T]] = {
        case (Right(v), _) => F.pure(Some(v))
        case (Left(_), true) => F.map(decoded)(Some(_))
        case (_, false) => F.pure(None)
      }

      for {
        att <- F.attempt(decoded)
        lu <- lookedUp
        ret <- evaluateOptional(att, lu)
      } yield ret
    }

  protected def resolveValue[F[_], E, T](
    parser: String => F[T]
  )(implicit hints: Hint, AE: Eff[F]): (List[String], Option[T], DecodeData) => F[T] =
    resolve[F, T, String](parser, lookupValue[F])

  protected def resolve[F[_], T, V](
    parser: V => F[T],
    lookup: (List[String], DecodeData) => F[Option[V]]
  )(path: List[String], default: Option[T], data: DecodeData)(implicit hints: Hint, F: Eff[F]): F[T] =
    for {
      v <- lookup(path, data)
      parsed <- Traverse[Option].sequence(v.map(parser))
      result <- selectOption(path, parsed, default)
    } yield result

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

  protected def formatParserError1[F[_], T](parseResult: Either[String, T])(implicit hints: Hint, F: Eff[F]): F[T] =
    parseResult.fold[F[T]](err => F.validationFailure(err), F.pure)

}

trait MultiParser[T] extends {
  def parse[F[_]: Monad](
    path: List[String],
    lookup: List[String] => OptionT[F, String]
  ): OptionT[F, ValidatedNel[String, T]]
}

object MultiParser {
  def apply[T](implicit multiParser: MultiParser[T]): MultiParser[T] = multiParser
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

  def convertTraversable[T, F[T] <: TraversableOnce[T]](
    li: TraversableOnce[T]
  )(implicit cbf: CanBuildFrom[F[T], T, F[T]]): F[T] =
    li.foldLeft(cbf())(_ += _).result()

  def traversableBuilder[T, F[T] <: TraversableOnce[T]](
    split: String => List[String]
  )(implicit parser: Parser[T], cbf: CanBuildFrom[F[T], T, F[T]]): Parser[F[T]] =
    Parser(
      input =>
        Traverse[List]
          .sequence[Either[String, ?], T](split(input).filterNot(_.isEmpty).map(parser.parse))
          .map(convertTraversable(_))
    )

  implicit def traversable[T, F[T] <: TraversableOnce[T]](
    implicit parser: Parser[T],
    cbf: CanBuildFrom[F[T], T, F[T]]
  ): Parser[F[T]] =
    traversableBuilder[T, F](_.split(',').toList)
}

case class Parser[T](parse: String => Either[String, T]) {
  def parseNel: String => ValidatedNel[String, T] = parse.andThen(_.toValidatedNel)
}

object Parser extends Parsers {
  def apply[T](implicit parser: Parser[T]): Parser[T] = parser

  def fromEitherException[T](parse: String => Either[Throwable, T]): Parser[T] =
    Parser[T](parse.andThen(_.leftMap(_.getMessage)))
}
