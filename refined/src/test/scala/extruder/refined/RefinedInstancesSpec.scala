package extruder.refined

import cats.Eq
import cats.instances.all._
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.numeric._
import eu.timepit.refined.scalacheck.numeric._
import extruder.core._
import extruder.data.Validation
import extruder.laws.EncoderDecoderTests
import extruder.map._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import shapeless.nat._0

class RefinedInstancesSpec extends FunSuite with Discipline {
  implicit def refinedEq[A: Eq, F[_, _], P](implicit refType: RefType[F]): Eq[F[A, P]] =
    Eq.by[F[A, P], A](refType.unwrap)

  checkAll(
    "Can encode and decode Int Refined Positive",
    EncoderDecoderTests[Validation, Settings, Map[String, String], Map[String, String], Map[String, String]](
      defaultSettings
    ).encodeDecode[Int Refined Positive]
  )

  test("Can derive a refined meta info") {
    val metaInfo = implicitly[RefinedMetaInfo[Int, Refined, Greater[_0]]]
    assert(metaInfo.`type` === "Refined[Int, Greater[_0]]")
    assert(metaInfo.underlying.`type` === "Int")
  }
}
