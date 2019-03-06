package extruder.core

import cats.Id
import cats.data.Validated
import org.scalatest.FunSuite

import scala.util.Try

class CombinedRefuteSuite extends FunSuite {
  import CombinedRefuteSuite._

  test("No implicit instance when no parser or multi-parser instance is found") {
    assertDoesNotCompile("""
        |CombinedRefute[TestA]
      """.stripMargin)
  }

  test("Implicit instance created when parser instance is found") {
    assertCompiles("""
        |CombinedRefute[TestB]
      """.stripMargin)
  }

  test("Implicit instance created when multi-parser instance is found") {
    assertCompiles("""
        |CombinedRefute[TestC]
      """.stripMargin)
  }
}

object CombinedRefuteSuite {
  case class TestA(a: String)
  case class TestB(a: String)
  case class TestC(a: String)

  implicit val parser: Parser[TestA] = Parser.fromTry(a => Try(TestA(a)))
  implicit val multiParser: MultiParser[Id, TestB] = MultiParser.make { lookup =>
    lookup(List("a")).map(a => Validated.Valid(TestB(a)))
  }
}
