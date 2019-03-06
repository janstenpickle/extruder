package extruder.core

import cats.data.{OptionT, ValidatedNel}
import cats.{Functor, Monad}
import extruder.instances.MultiParserInstances

/**
  * Parse `A` from multiple string values
  *
  * Uses a lookup function when parsing to retrieve multiple string values from a data source.
  * If no values could be looked up an empty option will be returned. If values could be looked up
  * but validation failed errors will be collected on the left side of a `ValidatedNel` instance.
  * If validation was successful then the value `A` will be returned on the right side of a
  * `ValidatedNel`.
  *
  * Implicit `DeocderT` instances will be constructed from every `MultiParser` instance where
  * a `StringReader` instance also exists.
  *
  * @tparam F Functor in which to wrap  the result
  * @tparam A
  */
trait MultiParser[F[_], A] {
  def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, A]]
  def map[B](f: A => B)(implicit F: Functor[F]): MultiParser[F, B] =
    MultiParser.make[F, B]((parse _).andThen(_.map(_.map(f))))
}

object MultiParser extends MultiParserInstances {
  def apply[F[_], T](implicit multiParser: MultiParser[F, T]): MultiParser[F, T] = multiParser

  def make[F[_], T](f: (List[String] => OptionT[F, String]) => OptionT[F, ValidatedNel[String, T]]): MultiParser[F, T] =
    new MultiParser[F, T] {
      override def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, T]] =
        f(lookup)
    }

}
