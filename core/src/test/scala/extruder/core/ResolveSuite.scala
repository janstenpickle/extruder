package extruder.core

import extruder.data.{Validation, ValidationErrors}
import extruder.map._
import org.scalatest.{EitherValues, FunSuite}

class ResolveSuite extends FunSuite with EitherValues {
  test("Fails to decode with parser") {
    assert(
      DecoderT[Validation, Settings, Int, Map[String, String]]
        .read(List.empty, defaultSettings, None, Map("" -> "not an int"))
        .left
        .value === ValidationErrors
        .failure("Could not parse value 'not an int' at '': For input string: \"not an int\"")
    )
  }
}
