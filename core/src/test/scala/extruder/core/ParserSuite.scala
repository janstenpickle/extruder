package extruder.core

import org.scalatest.{EitherValues, FunSuite}

import scala.util.Try

class ParserSuite extends FunSuite with EitherValues {
  test("From try") {
    assert(Parser.fromTry(s => Try(s)).parse("test").right.value === "test")
  }

  test("Catch non fatal") {
    assert(Parser.catchNonFatal(identity).parse("test").right.value === "test")
  }
}
