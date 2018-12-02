package extruder.core

import java.net.URL

import cats.{Functor, Traverse}
import cats.data.{NonEmptyList, OptionT, ValidatedNel}
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import extruder.instances.{MultiParserInstances, ParserInstances}
import mouse.string._
import shapeless.syntax.typeable._
import shapeless.{Lazy, LowPriority, Refute, Typeable}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

trait PrimitiveDecoders {
  self: Decoders with DecodeTypes =>
  protected def hasValue[F[_]](path: List[String], settings: Sett, data: DecodeData)(implicit F: Eff[F]): F[Boolean]

  protected def lookupValue[F[_]](path: List[String], settings: Sett, data: DecodeData)(
    implicit F: Eff[F]
  ): F[Option[String]]

  implicit def parserDecoder[F[_], T](
    implicit parser: Parser[T],
    F: Eff[F],
    error: ExtruderErrors[F],
    lp: LowPriority
  ): Dec[F, T] =
    mkDecoder[F, T](
      (path, settings, default, data) =>
        resolveValue(formatParserError(parser, path, settings)).apply(path, settings, default, data)
    )

  implicit def optionalMultiParserDecoder[F[_], T](
    implicit parser: MultiParser[F, T],
    F: Eff[F],
    extruderErrors: ExtruderErrors[F]
  ): Dec[F, Option[T]] =
    mkDecoder[F, Option[T]]((path, settings, _, data) => multiParse(parser, path, settings, data))

  implicit def multiParserDecoder[F[_], T](
    implicit parser: MultiParser[F, T],
    F: Eff[F],
    extruderErrors: ExtruderErrors[F],
    lp: LowPriority
  ): Dec[F, T] =
    mkDecoder[F, T](
      (path, settings, default, data) =>
        for {
          parsed <- multiParse(parser, path, settings, data)
          result <- selectOption(path, settings, parsed, default)
        } yield result
    )

  private def multiParse[F[_], T](parser: MultiParser[F, T], path: List[String], settings: Sett, data: DecodeData)(
    implicit F: Eff[F],
    error: ExtruderErrors[F]
  ): F[Option[T]] =
    parser
      .parse(p => OptionT(lookupValue[F](path ++ p, settings, data)))
      .value
      .flatMap(
        v =>
          Traverse[Option]
            .sequence[ValidatedNel[String, ?], T](v)
            .fold[F[Option[T]]](
              errs =>
                errs.tail
                  .foldLeft(error.validationFailure[Option[T]](errs.head))(
                    (acc, v) =>
                      F.ap2[Option[T], Option[T], Option[T]](F.pure((l, _) => l))(acc, error.validationFailure(v))
                ),
              F.pure
          )
      )

  implicit def nonEmptyListDecoder[F[_], T](
    implicit decoder: Lazy[Dec[F, List[T]]],
    F: Eff[F],
    error: ExtruderErrors[F]
  ): Dec[F, NonEmptyList[T]] = mkDecoder[F, NonEmptyList[T]] { (path, settings, default, data) =>
    val decoded = decoder.value.read(path, settings, default.map(_.toList), data)
    decoded
      .map(NonEmptyList.fromList)
      .flatMap[NonEmptyList[T]](
        _.fold[F[NonEmptyList[T]]](
          error.validationFailure(
            s"List at '${settings.pathToString(path)}' must contain data, but is empty, and no default available"
          )
        )(F.pure)
      )
  }

  implicit def optionalDecoder[F[_], T](
    implicit decoder: Lazy[Dec[F, T]],
    F: Eff[F],
    error: ExtruderErrors[F],
    refute: Refute[MultiParser[F, T]]
  ): Dec[F, Option[T]] =
    mkDecoder[F, Option[T]] { (path, settings, _, data) =>
      val decoded = decoder.value.read(path, settings, None, data)
      val lookedUp = hasValue[F](path, settings, data)

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
  )(implicit F: Eff[F], error: ExtruderErrors[F]): (List[String], Sett, Option[T], DecodeData) => F[T] =
    resolve[F, T, String](parser, lookupValue[F])

  protected def resolve[F[_], T, V](parser: V => F[T], lookup: (List[String], Sett, DecodeData) => F[Option[V]])(
    path: List[String],
    settings: Sett,
    default: Option[T],
    data: DecodeData
  )(implicit F: Eff[F], error: ExtruderErrors[F]): F[T] =
    for {
      v <- lookup(path, settings, data)
      parsed <- Traverse[Option].sequence(v.map(parser))
      result <- selectOption(path, settings, parsed, default)
    } yield result

  protected def formatParserError[F[_], T](parser: Parser[T], path: List[String], settings: Sett)(
    implicit F: Eff[F],
    error: ExtruderErrors[F]
  ): String => F[T] =
    value =>
      parser
        .parse(value)
        .fold[F[T]](
          err => error.validationFailure(s"Could not parse value '$value' at '${settings.pathToString(path)}': $err"),
          F.pure
      )

  protected def formatParserError1[F[_], T](
    parseResult: Either[String, T],
    settings: Sett
  )(implicit F: Eff[F], error: ExtruderErrors[F]): F[T] =
    parseResult.fold[F[T]](err => error.validationFailure(err), F.pure)

}

trait MultiParser[F[_], T] {
  def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, T]]
  def map[A](f: T => A)(implicit F: Functor[F]): MultiParser[F, A] =
    MultiParser.parser[F, A]((parse _).andThen(_.map(_.map(f))))
}

object MultiParser extends MultiParserInstances {
  def apply[F[_], T](implicit multiParser: MultiParser[F, T]): MultiParser[F, T] = multiParser
  def parser[F[_], T](
    f: (List[String] => OptionT[F, String]) => OptionT[F, ValidatedNel[String, T]]
  ): MultiParser[F, T] =
    new MultiParser[F, T] {
      override def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, T]] =
        f(lookup)
    }
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

trait Parser[T] {
  def parse: String => Either[String, T]
  def parseNel: String => ValidatedNel[String, T] = parse.andThen(_.toValidatedNel)
  def map[A](f: T => A): Parser[A] = Parser[A](parse.andThen(_.map(f)))
  def flatMapResult[A](f: T => Either[String, A]): Parser[A] = Parser(parse.andThen(_.flatMap(f)))
}

object Parser extends Parsers with ParserInstances {
  def apply[T](f: String => Either[String, T]): Parser[T] = new Parser[T] {
    override def parse: String => Either[String, T] = f
  }

  def apply[T](implicit parser: Parser[T]): Parser[T] = parser

  def fromEitherException[T](parse: String => Either[Throwable, T]): Parser[T] =
    Parser[T](parse.andThen(_.leftMap(_.getMessage)))

  def catchNonFatal[T](parse: String => T): Parser[T] =
    Parser.fromEitherException[T](str => Either.catchNonFatal(parse(str)))
}
