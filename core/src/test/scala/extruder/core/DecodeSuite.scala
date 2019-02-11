package extruder.core

import cats.instances.try_._
import extruder.data.Validation
import org.scalatest.{EitherValues, FunSuite}
import extruder.map._

import scala.util.Try

class DecodeSuite extends FunSuite with EitherValues {
  import DecodeSuite._

  test("Can run a decoder with the default monad type") {
    assert(decode[String](Map("" -> "test")).right.value === "test")
  }

  test("Can run a decoder with a custom monad type") {
    assert(decodeF[Try, String](Map("" -> "test")).get === "test")
  }

  test("Can run a decoder with default data") {
    assert(decode[String]().right.value === "test")
  }

  test("Can run a decoder with default data and custom settings") {
    assert(decode[String](customSettings).right.value === "test")
  }

  test("Can run a decoder with default data and custom namespace") {
    assert(decode[String](List("")).right.value === "test")
  }

  test("Can run a decoder with default data and custom namespace and settings") {
    assert(decode[String](List(""), customSettings).right.value === "test")
  }

  test("Can run a decoder with custom namespace") {
    assert(decode[String](List("value"), Map("value" -> "test")).right.value === "test")
  }

  test("Can run a decoder with custom settings") {
    assert(decode[String](customSettings, Map("" -> "test")).right.value === "test")
  }

  test("Can run a decoder with custom namespace and settings") {
    assert(decode[String](List("value"), customSettings, Map("value" -> "test")).right.value === "test")
  }

  test("Combine with provided settings") {
    assert(
      decode[String]
        .combine[Settings, Map[String, String], Map[String, String]](customSettings)(
          (Map.empty[String, String], Map("" -> "test"))
        )
        .right
        .value === "test"
    )
  }

  test("Combine with both provided settings") {
    assert(
      decode[String]
        .combine[Settings, Map[String, String], Map[String, String]](
          (customSettings, customSettings),
          (Map.empty[String, String], Map("" -> "test"))
        )
        .right
        .value === "test"
    )
  }

  test("Combine with another decode with default settings") {
    assert(
      decode[String]
        .combine(decode[String])((Map.empty[String, String], Map("" -> "test")))
        .right
        .value === "test"
    )
  }

  test("Combine with another datasource") {
    assert(
      decode[String]
        .combine(extruder.map.datasource)((Map.empty[String, String], Map("" -> "test")))
        .right
        .value === "test"
    )
  }
}

object DecodeSuite {
  val customSettings = new Settings {
    override def pathToString(path: List[String]): String = path.mkString(".")
  }

  implicit val loadInput: LoadInput[Validation, Map[String, String]] = new LoadInput[Validation, Map[String, String]] {
    override def load: Validation[Map[String, String]] = Validation(Right(Map("" -> "test")))
  }
}
