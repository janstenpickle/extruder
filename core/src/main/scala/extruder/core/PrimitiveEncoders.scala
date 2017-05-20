package extruder.core

import java.net.URL

import cats.instances.all._
import cats.syntax.validated._
import cats.{Show => CatsShow}
import shapeless._

import scala.concurrent.duration.Duration

trait PrimitiveEncoders[C, E[T] <: Encoder[T, C]] { self: Encoders[C, E] with UtilsMixin =>
  protected def writeValue(path: Seq[String], value: String): ConfigValidation[C]

  implicit def primitiveEncoder[T](implicit shows: Show[T]): E[T] =
    mkEncoder[T]((path, value) => writeValue(path, shows.show(value)))

  implicit def optionalEncoder[T](implicit encoder: Lazy[E[T]]): E[Option[T]] =
    mkEncoder[Option[T]]((path, value) =>
      value.fold[ConfigValidation[C]](monoid.empty.validNel)(encoder.value.write(path, _))
    )

  implicit def traversableEncoder[T, F[T] <: TraversableOnce[T]](implicit shows: Show[T]): E[F[T]] =
    mkEncoder((path, value) => writeValue(path, value.map(shows.show).filter(_.trim.nonEmpty).mkString(utils.ListSeparator)))
}

trait Shows {
  implicit val charShow: Show[Char] = Show(catsStdShowForChar)
  implicit val stringShow: Show[String] = Show(catsStdShowForString)
  implicit val intShow: Show[Int] = Show(catsStdShowForInt)
  implicit val longShow: Show[Long] = Show(catsStdShowForLong)
  implicit val doubleShow: Show[Double] = Show(catsStdShowForDouble)
  implicit val floatShow: Show[Float] = Show(catsStdShowForFloat)
  implicit val shortShow: Show[Short] = Show(catsStdShowForShort)
  implicit val byteShow: Show[Byte] = Show(catsStdShowForByte)
  implicit val booleanShow: Show[Boolean] = Show(catsStdShowForBoolean)
  implicit val urlShow: Show[URL] = Show(CatsShow.fromToString[URL])
  implicit def durationShow[T <: Duration]: Show[T] = Show(CatsShow.show[T] {
    case x: Duration if x == Duration.Inf => "Inf"
    case x: Duration if x == Duration.MinusInf => "MinusInf"
    case x: Any => x.toString
  })
}

case class Show[T](show: T => String)

object Show extends Shows {
  def apply[T](implicit showWrapper: Show[T]): Show[T] = showWrapper
  def apply[T](show: CatsShow[T]): Show[T] = new Show(show.show)
}
