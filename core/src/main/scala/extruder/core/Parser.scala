package extruder.core

import java.net.URL

import cats.Traverse
import cats.data.ValidatedNel
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import extruder.instances.ParserInstances
import mouse.string._
import shapeless.Typeable
import shapeless.syntax.typeable._

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait Parser[A] {
  def parse: String => Either[String, A]
  def parseNel: String => ValidatedNel[String, A] = parse.andThen(_.toValidatedNel)
  def map[B](f: A => B): Parser[B] = Parser[B](parse.andThen(_.map(f)))
  def flatMapResult[B](f: A => Either[String, B]): Parser[B] = Parser(parse.andThen(_.flatMap(f)))
}

object Parser extends Parsers with ParserInstances {
  def apply[A](f: String => Either[String, A]): Parser[A] = new Parser[A] {
    override def parse: String => Either[String, A] = f
  }

  def apply[A](implicit parser: Parser[A]): Parser[A] = parser

  def fromEitherException[A](parse: String => Either[Throwable, A]): Parser[A] =
    Parser[A](parse.andThen(_.leftMap(_.getMessage)))

  def fromTry[A](f: String => Try[A]): Parser[A] =
    fromEitherException(f.andThen {
      case Success(a) => Right(a)
      case Failure(ex) => Left(ex)
    })

  def catchNonFatal[A](parse: String => A): Parser[A] =
    Parser.fromEitherException[A](str => Either.catchNonFatal(parse(str)))
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
          .sequence[Either[String, ?], T](split(input).filterNot(_.isEmpty).map(v => parser.parse(v.trim)))
          .map(convertTraversable(_))
    )

  implicit def traversable[T, F[T] <: TraversableOnce[T]](
    implicit parser: Parser[T],
    cbf: CanBuildFrom[F[T], T, F[T]]
  ): Parser[F[T]] =
    traversableBuilder[T, F](_.split(',').toList)
}
