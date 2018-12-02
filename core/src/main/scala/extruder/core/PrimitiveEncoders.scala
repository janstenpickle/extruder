package extruder.core

import java.net.URL

import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.functor._
import cats.{Contravariant, Traverse, Show => CatsShow}
import extruder.instances.{MultiShowInstances, ShowInstances}
import shapeless._

import scala.concurrent.duration.Duration

trait PrimitiveEncoders { self: Encoders with EncodeTypes =>
  protected def writeValue[F[_]](path: List[String], settings: Sett, value: String)(implicit F: Eff[F]): F[EncodeData]

  implicit def showEncoder[F[_], T](implicit shows: Show[T], F: Eff[F], lp: LowPriority): Enc[F, T] =
    mkEncoder[F, T] { (path, settings, value) =>
      writeValue(path, settings, shows.show(value))
    }

  implicit def multiShowEncoder[F[_], T](implicit shows: MultiShow[T], F: Eff[F], lp: LowPriority): Enc[F, T] =
    mkEncoder[F, T] { (path, settings, value) =>
      Traverse[List]
        .traverse(shows.show(value).toList) { case (p, v) => writeValue[F](path ++ p, settings, v) }
        .map(monoid.combineAll)
    }

  implicit def nonEmptyListEncoder[F[_], T](implicit encoder: Lazy[Enc[F, List[T]]]): Enc[F, NonEmptyList[T]] =
    mkEncoder { (path, settings, value) =>
      encoder.value.write(path, settings, value.toList)
    }

  implicit def optionalEncoder[F[_], T](implicit encoder: Lazy[Enc[F, T]], F: Eff[F]): Enc[F, Option[T]] =
    mkEncoder[F, Option[T]] { (path, settings, value) =>
      value.fold[F[EncodeData]](F.pure(monoid.empty))(encoder.value.write(path, settings, _))
    }
}

trait MultiShow[T] {
  def show(v: T): Map[List[String], String]
}

object MultiShow extends MultiShowInstances {
  def apply[T](implicit multiShow: MultiShow[T]): MultiShow[T] = multiShow
  def by[T, V](f: V => T)(implicit ev: MultiShow[T]): MultiShow[V] =
    Contravariant[MultiShow].contramap(ev)(f)
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
    Show.make((x: FF[T]) => concat(x.map(shows.show).filter(_.trim.nonEmpty)))

  implicit def traversable[T, F[_], FF[T] <: TraversableOnce[T]](implicit shows: Show[T]): Show[FF[T]] =
    traversableBuilder[T, F, FF](_.mkString(","))
}

trait Show[T] {
  def show(t: T): String
}

object Show extends Shows with ShowInstances {
  def make[T](f: T => String): Show[T] = new Show[T] {
    override def show(t: T): String = f(t)
  }
  def apply[T](implicit show: Show[T]): Show[T] = show
  def apply[T](show: CatsShow[T]): Show[T] = make[T](show.show _)
  def by[T, V](f: V => T)(implicit ev: Show[T]): Show[V] =
    Contravariant[Show].contramap(ev)(f)
}
