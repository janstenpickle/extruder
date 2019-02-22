package extruder.circe.yaml

import extruder.data.ValidationErrors
import io.circe.syntax._
import io.circe.yaml.printer
import org.scalatest.{EitherValues, FunSuite}

class CirceYamlDataSourceSuite extends FunSuite with EitherValues {
  import CirceYamlDataSourceSuite._

  test("Decodes simple value") {
    assert(decode[Int]("1").right.value === 1)
  }

  test("Encodes simple value") {
    assert(encode(1).stripLineEnd === "1")
  }

  test("Decodes nested value") {
    assert(decode[Int](nestedPath, nestedYaml).right.value === 1)
  }

  test("Encodes nested value") {
    assert(encode(nestedPath, 1) === nestedYaml)
  }

  test("Encode decode nested values") {
    assert(decode[Int](nestedPath, encode(nestedPath, 1)).right.value === 1)
  }

  test("Can decode an empty option") {
    assert(decode[Option[Int]]("---").right.value === None)
  }

  test("Fails to decode missing a value") {
    assert(
      decode[Int]("---").left.value === ValidationErrors
        .missing(s"Could not find value at '' and no default available")
    )
  }
}

object CirceYamlDataSourceSuite {
  val nestedPath: List[String] = List("a", "b", "c")
  val nestedYaml: String = printer.print(Map("a" -> Map("b" -> Map("c" -> 1.asJson).asJson).asJson).asJson)
}
