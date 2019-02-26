package extruder.circe

import cats.instances.either._
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.kernel.laws.discipline.MonoidTests
import extruder.core.DecoderT
import extruder.data.{Validation, ValidationErrors}
import extruder.laws.EncoderDecoderGenericTests
import io.circe.Json
import io.circe.syntax._
import org.scalacheck.Arbitrary
import org.scalatest.{EitherValues, FunSuite}
import org.typelevel.discipline.scalatest.Discipline

class CirceDataSourceSuite extends FunSuite with Discipline with EitherValues {
  import CirceDataSourceSuite._

  checkAll("Json Monoid", MonoidTests[Json].monoid)

  // have to use `asInstanceOf` to get the compiler to work on 2.11
  checkAll(
    "Circe data source derived test",
    EncoderDecoderGenericTests[Validation, CirceSettings, Json, Json, Json](defaultSettings.asInstanceOf[CirceSettings])
      .derivedEncodeDecode[Int, String]
  )

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

  test("Can decode a JSON array") {
    assert(decode[List[Map[String, String]]](jsonList).right.value === listData)
  }

  test("Can encode a JSON array") {
    assert(encode(listData) === jsonList)
  }

}

object CirceDataSourceSuite {
  val nestedPath: List[String] = List("a", "b", "c")
  val nestedJson: Json = Map("a" -> Map("b" -> Map("c" -> 1.asJson).asJson).asJson).asJson
  val listData = List(Map("a" -> "b"), Map("c" -> "d"))
  val jsonList: Json = listData.asJson

  implicit val jsonArb: Arbitrary[Json] = Arbitrary(implicitly[Arbitrary[Map[String, String]]].arbitrary.map(_.asJson))
}
