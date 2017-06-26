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
  protected def hasValue[F[_], E](path: Seq[String], config: DecodeConfig)(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Boolean]]
  protected def lookupValue[F[_], E](path: Seq[String], config: DecodeConfig)(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]]
  protected def lookupList[F[_], E](path: Seq[String], config: DecodeConfig)(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[List[String]]]] =
    lookupValue[F, E](path, config).map(_.map(_.map(_.split(utils.ListSeparator).toList.map(_.trim))))

  implicit def primitiveDecoder[F[_], E, T](implicit parser: Parser[T], utils: Hint, AE: ExtruderApplicativeError[F, E]): Dec[F, T] =
    mkDecoder[F, T]((path, default, config) => resolveValue(formatParserError(parser, path)).apply(path, default, config))

  implicit def traversableDecoder[F[_], E, T, FF[T] <: TraversableOnce[T]](implicit parser: Parser[T],
                                                                           cbf: CanBuildFrom[FF[T], T, FF[T]],
                                                                           utils: Hint,
                                                                           AE: ExtruderApplicativeError[F, E]): Dec[F, FF[T]] =
    mkDecoder((path, default, config) =>
      resolveList { x =>
        val seq: F[List[T]] = AE.sequence(x.filterNot(_.isEmpty).map(formatParserError(parser, path)))

        AE.map(seq)(_.foldLeft(cbf())(_ += _).result())
      }.apply(path, default, config)
    )

  implicit def optionalDecoder[F[_], E, T](implicit decoder: Lazy[Dec[F, T]], utils: Hint, AE: ExtruderApplicativeError[F, E]): Dec[F, Option[T]] =
    mkDecoder[F, Option[T]] { (path, _, config) =>
      val evaluateOptional: (Either[E, T], Boolean) => F[Option[T]] = {
        case (Right(v), _) => AE.pure(Some(v))
        case (Left(e), true) => AE.raiseError(e)
        case (_, false) => AE.pure(None)
      }

      for {
        attempted <- decoder.value.read(path, None, config).map(AE.attempt)
        lookedUp <- hasValue[F, E](path, config)
      } yield AE.flatMap(attempted)(att =>
        AE.flatMap(lookedUp)(lu => evaluateOptional(att, lu))
      )
    }

  protected def resolveValue[F[_], E, T](parser: String => F[T])(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): (Seq[String], Option[T], DecodeConfig) => IO[F[T]] =
    resolve[F, E, T, String](parser, lookupValue[F, E])

  protected def resolveList[F[_], E, T](parser: List[String] => F[T])(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): (Seq[String], Option[T], DecodeConfig) => IO[F[T]] =
    resolve[F, E, T, List[String]](parser, lookupList[F, E])

  protected def resolve[F[_], E, T, V](parser: V => F[T],
                                       lookup: (Seq[String], DecodeConfig) => IO[F[Option[V]]])(path: Seq[String], default: Option[T], config: DecodeConfig)
                                      (implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[T]] = {

    lookup(path, config).map(AE.flatMap[Option[V], T](_)(v => (v, default) match {
      case (None, None) => AE.missing(s"Could not find configuration at '${utils.pathToString(path)}' and no default available")
      case (None, Some(value)) => AE.pure(value)
      case (Some(value), _) => parser(value)
    }))
  }

  protected def formatParserError[F[_], E, T](parser: Parser[T], path: Seq[String])(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): String => F[T] = value =>
    parser.parse(value).fold[F[T]](
      err => AE.validationFailure(s"Could not parse value '$value' at '${utils.pathToString(path)}': $err"),
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
  implicit def duration[T <: Duration: Typeable: ClassTag](implicit ct: ClassTag[T]): Parser[T] = Parser(dur =>
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
