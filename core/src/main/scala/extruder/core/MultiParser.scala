package extruder.core

import cats.data.{OptionT, ValidatedNel}
import cats.{Functor, Monad}
import extruder.instances.MultiParserInstances

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

  implicit def tuple2MultiParser[F[_]: Monad, A: Parser, B: Parser]: MultiParser[F, (A, B)] =
    new MultiParser[F, (A, B)] {
      override def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, (A, B)]] =
        for {
          a <- lookup(List("_1"))
          b <- lookup(List("_2"))
        } yield Parser[A].parseNel(a).product(Parser[B].parseNel(b))
    }
}
