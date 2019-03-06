package extruder.core

import extruder.data.Validation
import org.scalatest.{EitherValues, FunSuite}
import extruder.map._

class EncodeSuite extends FunSuite with EitherValues {
  import EncodeSuite._

  test("Can run encoder with default monad type") {
    assert(encode("test") === Map("" -> "test"))
  }

  test("Can run encoder with custom monad type") {
    assert(encodeF[Validation]("test").right.value === Map("" -> "test"))
  }

  test("Can run encoder with custom settings") {
    assert(encode(customSettings, "test") === Map("" -> "test"))
  }

  test("Can run encoder with custom namespace") {
    assert(encode(List("value"), "test") === Map("value" -> "test"))
  }

  test("Can run encoder with custom namespace and settings") {
    assert(encode(List("value"), customSettings, "test") === Map("value" -> "test"))
  }

  test("Combine with provided settings") {
    assert(
      encodeF[Validation]
        .combine[Settings, Map[String, String], Map[String, String]](customSettings)("test")
        .right
        .value
        .isBoth
    )
  }

  test("Combine with both provided settings") {
    assert(
      encodeF[Validation]
        .combine[Settings, Map[String, String], Map[String, String]]((customSettings, customSettings), "test")
        .right
        .value
        .isBoth
    )
  }

  test("Combine with another encode with default settings") {
    assert(
      encodeF[Validation]
        .combine[Settings, Map[String, String], Map[String, String]](encodeF[Validation])("test")
        .right
        .value
        .isBoth
    )
  }

  test("Combine with another datasource") {
    assert(
      encodeF[Validation]
        .combine(extruder.map.datasource)("test")
        .right
        .value
        .isBoth
    )
  }
}

object EncodeSuite {
  val customSettings = new Settings {
    override def pathToString(path: List[String]): String = path.mkString(".")
  }
}
