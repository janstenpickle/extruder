package extruder.core

import cats.data.NonEmptyChain
import extruder.data.{Validation, ValidationErrors}
import extruder.map._
import org.scalatest.{EitherValues, FunSuite}

class DerivedDecoderSuite extends FunSuite with EitherValues {
  test("Fails to decode a non-empty chain") {
    assert(
      Decoder[Validation, Settings, NonEmptyChain[Int], Map[String, String]]
        .read(List.empty, defaultSettings, None, Map("" -> ""))
        .left
        .value === ValidationErrors
        .failure("Collection at '' must contain data, but is empty, and no default available")
    )
  }
}
