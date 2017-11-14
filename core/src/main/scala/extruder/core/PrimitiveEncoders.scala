package extruder.core

import java.net.URL

import cats.instances.all._
import cats.{Show => CatsShow}
import shapeless._

import scala.concurrent.duration.Duration

trait PrimitiveEncoders { self: Encoders with EncodeTypes =>
  protected def writeValue[F[_]](path: List[String], value: String)(implicit hints: Hint, F: Eff[F]): F[EncodeData]

  implicit def primitiveEncoder[F[_], T](implicit shows: Show[T], hints: Hint, F: Eff[F]): Enc[F, T] =
    mkEncoder[F, T] { (path, value) =>
      writeValue(path, shows.show(value))
    }

  implicit def optionalEncoder[F[_], T](implicit encoder: Lazy[Enc[F, T]], hints: Hint, F: Eff[F]): Enc[F, Option[T]] =
    mkEncoder[F, Option[T]] { (path, value) =>
      value.fold[F[EncodeData]](F.pure(monoid.empty))(encoder.value.write(path, _))
    }

  implicit def traversableEncoder[F[_], T, FF[T] <: TraversableOnce[T]](
    implicit shows: Show[T],
    hints: Hint,
    F: Eff[F]
  ): Enc[F, FF[T]] =
    mkEncoder { (path, value) =>
      writeValue(path, value.map(shows.show).filter(_.trim.nonEmpty).mkString(hints.ListSeparator))
    }
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
  implicit def durationShow[T <: Duration]: Show[T] =
    Show(CatsShow.show[T] {
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
