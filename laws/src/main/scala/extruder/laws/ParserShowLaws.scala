package extruder.laws

import cats.laws._
import extruder.core.{Parser, Show}

trait ParserShowLaws[A] {
  def parser: Parser[A]
  def show: Show[A]

  def showParse(a: A): IsEq[Either[String, A]] =
    parser.parse(show.show(a)) <-> Right(a)
}

object ParserShowLaws {
  def apply[A: Parser: Show]: ParserShowLaws[A] = new ParserShowLaws[A] {
    override def parser: Parser[A] = Parser[A]
    override def show: Show[A] = Show[A]
  }
}
