package extruder.circe.yaml

import cats.instances.either._
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import extruder.circe.CirceSettings
import extruder.data.{Validation, ValidationErrors}
import extruder.laws.EncoderDecoderGenericTests
import io.circe.Json
import io.circe.syntax._
import io.circe.yaml.printer
import org.scalatest.{EitherValues, FunSuite}
import org.typelevel.discipline.scalatest.Discipline

class CirceYamlDataSourceSuite extends FunSuite with Discipline with EitherValues {
  import CirceYamlDataSourceSuite._

  // have to use `asInstanceOf` to get the compiler to work on 2.11
  checkAll(
    "Circe data source derived test",
    EncoderDecoderGenericTests[Validation, CirceSettings, Json, Json, Json](defaultSettings.asInstanceOf[CirceSettings])
      .derivedEncodeDecode[Int, String]
  )

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
