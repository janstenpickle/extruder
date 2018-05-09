package extruder.instances

import cats.Functor
import extruder.core.Parser

trait ParserInstances {
  implicit val extruderStdInstancesForParser: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa.map(f)
  }
}
