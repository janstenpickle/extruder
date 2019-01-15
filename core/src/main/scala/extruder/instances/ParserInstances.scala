package extruder.instances

import cats.Alternative
import cats.syntax.either._
import extruder.core.Parser

trait ParserInstances {
  implicit val extruderStdInstancesForParser: Alternative[Parser] = new Alternative[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa.map(f)

    override def empty[A]: Parser[A] = Parser(Function.const(Left("Empty input"))(_))

    override def pure[A](x: A): Parser[A] = Parser(Function.const(Right(x))(_))

    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] =
      new Parser[A] {
        override val parse: String => Either[String, A] = in => x.parse(in).orElse(y.parse(in))
      }

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      new Parser[B] {
        override val parse: String => Either[String, B] = in => fa.parse(in).ap(ff.parse(in))
      }
  }
}
