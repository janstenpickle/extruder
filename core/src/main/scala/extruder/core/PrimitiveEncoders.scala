package extruder.core

import java.net.URL

import cats.effect.IO
import cats.instances.all._
import cats.{Show => CatsShow}
import shapeless._

import scala.concurrent.duration.Duration

trait PrimitiveEncoders { self: Encoders with EncodeTypes =>
  protected def writeValue[F[_], E](path: Seq[String], value: String)(
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): IO[F[EncodeConfig]]

  implicit def primitiveEncoder[F[_], E, T](
    implicit shows: Show[T],
    utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): Enc[F, T] =
    mkEncoder[F, T] { (path, value) =>
      writeValue(path, shows.show(value))
    }

  implicit def optionalEncoder[F[_], E, T](
    implicit encoder: Lazy[Enc[F, T]],
    utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): Enc[F, Option[T]] =
    mkEncoder[F, Option[T]] { (path, value) =>
      value.fold[IO[F[EncodeConfig]]](IO(AE.pure(monoid.empty)))(encoder.value.write(path, _))
    }

  implicit def traversableEncoder[F[_], E, T, FF[T] <: TraversableOnce[T]](
    implicit shows: Show[T],
    utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): Enc[F, FF[T]] =
    mkEncoder { (path, value) =>
      writeValue(path, value.map(shows.show).filter(_.trim.nonEmpty).mkString(utils.ListSeparator))
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
