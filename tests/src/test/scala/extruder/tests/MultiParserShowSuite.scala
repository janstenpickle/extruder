package extruder.tests

import cats.{Eq, Monad}
import cats.data.{OptionT, ValidatedNel}
import cats.instances.all._
import extruder.core.{MultiParser, MultiShow, Parser, Show}
import extruder.data.Validation
import extruder.laws.MultiParserShowTests
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class MultiParserShowSuite extends FunSuite with Discipline {
  implicit val eq: Eq[OptionT[Validation, ValidatedNel[String, (Int, Int)]]] =
    OptionT.catsDataEqForOptionT[Validation, ValidatedNel[String, (Int, Int)]]

  implicit def tuple2MultiParser[F[_]: Monad, A: Parser, B: Parser]: MultiParser[F, (A, B)] =
    new MultiParser[F, (A, B)] {
      override def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, (A, B)]] =
        for {
          a <- lookup(List("_1"))
          b <- lookup(List("_2"))
        } yield Parser[A].parseNel(a).product(Parser[B].parseNel(b))
    }

  implicit def tuple2MultiShow[A: Show, B: Show]: MultiShow[(A, B)] = new MultiShow[(A, B)] {
    def show(a: (A, B)): Map[List[String], String] =
      Map(List("_1") -> Show[A].show(a._1), List("_2") -> Show[B].show(a._2))
  }

  checkAll("Int Tuple", MultiParserShowTests[Validation, (Int, Int)].parserShow)
}
