package extruder.core

import extruder.data.{Validation, ValidationErrors}
import org.scalatest.{EitherValues, FunSuite}
import extruder.map._

class GenericDecoderSuite extends FunSuite with EitherValues {
  test("Fails to find a specific implementation when specified via type name") {
    assert(
      Decoder[Validation, Settings, Sealed, Map[String, String]]
        .read(List.empty, defaultSettings, None, Map("type" -> "invalid"))
        .left
        .value === ValidationErrors.failure("Could not find specified implementation of sealed type at path 'type'")
    )
  }

  test("Fails to find a specific implementation when specified via type name, with combined input") {
    assert(
      Decoder[Validation, (Settings, Settings), Sealed, (Map[String, String], Map[String, String])]
        .read(
          List.empty,
          (defaultSettings, defaultSettings),
          None,
          (Map("type" -> "invalid"), Map("type" -> "invalid"))
        )
        .left
        .value === ValidationErrors.failure("Could not find specified implementation of sealed type")
    )
  }
}
