package extruder.core

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Try

class ParserSuite extends AnyFunSuite with EitherValues {
  test("From try") {
    assert(Parser.fromTry(s => Try(s)).parse("test").right.value === "test")
  }

  test("Catch non fatal") {
    assert(Parser.catchNonFatal(identity).parse("test").right.value === "test")
  }
}
