package extruder.core

import java.net.URL

import cats.instances.all._
import cats.{Show => CatsShow}
import extruder.instances.ShowInstances

import scala.concurrent.duration.Duration

/**
  * Encode value `A` as a string
  *
  * Implicit `EncoderT` instances will be constructed from every `Shows` instance where
  * a `StringWriter` instance also exists.
  *
  * @tparam A value to encode
  */
trait Show[A] {
  def show(a: A): String
  def contramap[B](f: B => A): Show[B] = Show.make((show _).compose(f))
}

object Show extends Shows with ShowInstances {
  def make[A](f: A => String): Show[A] = new Show[A] {
    override def show(t: A): String = f(t)
  }

  def apply[T](implicit show: Show[T]): Show[T] = show

  def by[B, V](f: V => B)(implicit ev: Show[B]): Show[V] =
    ev.contramap(f)
}

trait Shows extends LowPriorityShows {
  implicit val charShow: Show[Char] = fromCatsShow(catsStdShowForChar)
  implicit val stringShow: Show[String] = fromCatsShow(catsStdShowForString)
  implicit val intShow: Show[Int] = fromCatsShow(catsStdShowForInt)
  implicit val longShow: Show[Long] = fromCatsShow(catsStdShowForLong)
  implicit val doubleShow: Show[Double] = fromCatsShow(catsStdShowForDouble)
  implicit val floatShow: Show[Float] = fromCatsShow(catsStdShowForFloat)
  implicit val shortShow: Show[Short] = fromCatsShow(catsStdShowForShort)
  implicit val byteShow: Show[Byte] = fromCatsShow(catsStdShowForByte)
  implicit val booleanShow: Show[Boolean] = fromCatsShow(catsStdShowForBoolean)
  implicit val urlShow: Show[URL] = fromCatsShow(CatsShow.fromToString[URL])
  implicit def durationShow[A <: Duration]: Show[A] =
    fromCatsShow(CatsShow.show[A] {
      case x: Duration if x == Duration.Inf => "Inf"
      case x: Duration if x == Duration.MinusInf => "MinusInf"
      case x: Any => x.toString
    })

  implicit def optionalShow[A](implicit ev: Show[A]): Show[Option[A]] = Show.by(_.fold("")(ev.show))

  def traversableBuilder[T, F[_], FF[T] <: TraversableOnce[T]](
    concat: TraversableOnce[String] => String
  )(implicit shows: Show[T]): Show[FF[T]] =
    Show.make((x: FF[T]) => concat(x.map(shows.show).filter(_.trim.nonEmpty)))

  implicit def traversable[T, F[_], FF[T] <: TraversableOnce[T]](implicit shows: Show[T]): Show[FF[T]] =
    traversableBuilder[T, F, FF](_.mkString(","))
}

trait LowPriorityShows {
  implicit def fromCatsShow[T](implicit catsShow: CatsShow[T]): Show[T] = new Show[T] {
    override def show(t: T): String = catsShow.show(t)
  }
}
