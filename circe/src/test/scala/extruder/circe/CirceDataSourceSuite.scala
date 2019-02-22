package extruder.circe

import extruder.data.ValidationErrors
import io.circe.Json
import org.scalatest.{EitherValues, FunSuite}
import io.circe.syntax._

class CirceDataSourceSuite extends FunSuite with EitherValues {
  import CirceDataSourceSuite._

  test("Decodes simple value") {
    assert(decode[Int](1.asJson).right.value === 1)
  }

  test("Encodes simple value") {
    assert(encode(1) === 1.asJson)
  }

  test("Decodes nested value") {
    assert(decode[Int](nestedPath, nestedJson).right.value === 1)
  }

  test("Encodes nested value") {
    assert(encode(nestedPath, 1) === nestedJson)
  }

  test("Encode decode nested values") {
    assert(decode[Int](nestedPath, encode(nestedPath, 1)).right.value === 1)
  }

  test("Can decode an empty option") {
    assert(decode[Option[Int]](Json.Null).right.value === None)
  }

  test("Fails to decode missing a value") {
    assert(
      decode[Int](Json.Null).left.value === ValidationErrors
        .missing(s"Could not find value at '' and no default available")
    )
  }
}

object CirceDataSourceSuite {
  val nestedPath: List[String] = List("a", "b", "c")
  val nestedJson: Json = Map("a" -> Map("b" -> Map("c" -> 1.asJson).asJson).asJson).asJson
}
