package extruder.core

import java.net.URL

import cats.data.NonEmptyList
import cats.instances.all._
import cats.{Show => CatsShow}
import shapeless._

import scala.concurrent.duration.Duration

trait PrimitiveEncoders { self: Encoders with EncodeTypes =>
  protected def writeValue[F[_]](path: List[String], value: String)(implicit hints: Hint, F: Eff[F]): F[EncodeData]

  implicit def primitiveEncoder[F[_], T](implicit shows: Show[T], hints: Hint, F: Eff[F], lp: LowPriority): Enc[F, T] =
    mkEncoder[F, T] { (path, value) =>
      writeValue(path, shows.show(value))
    }

  implicit def nonEmptyListEncoder[F[_], T](implicit encoder: Lazy[Enc[F, List[T]]]): Enc[F, NonEmptyList[T]] =
    mkEncoder { (path, value) =>
      encoder.value.write(path, value.toList)
    }

  implicit def optionalEncoder[F[_], T](implicit encoder: Lazy[Enc[F, T]], hints: Hint, F: Eff[F]): Enc[F, Option[T]] =
    mkEncoder[F, Option[T]] { (path, value) =>
      value.fold[F[EncodeData]](F.pure(monoid.empty))(encoder.value.write(path, _))
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

  def traversableBuilder[T, F[_], FF[T] <: TraversableOnce[T]](
    concat: TraversableOnce[String] => String
  )(implicit shows: Show[T]): Show[FF[T]] =
    Show((x: FF[T]) => concat(x.map(shows.show).filter(_.trim.nonEmpty)))

  implicit def traversable[T, F[_], FF[T] <: TraversableOnce[T]](implicit shows: Show[T]): Show[FF[T]] =
    traversableBuilder[T, F, FF](_.mkString(","))
}

case class Show[T](show: T => String)

object Show extends Shows {
  def apply[T](implicit showWrapper: Show[T]): Show[T] = showWrapper
  def apply[T](show: CatsShow[T]): Show[T] = new Show(show.show)
}
