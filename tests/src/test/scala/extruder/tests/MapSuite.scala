package extruder.tests

import cats.instances.int._
import cats.instances.map.catsKernelStdEqForMap
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.list._
import cats.kernel.laws.discipline.MonoidTests
import extruder.core.Settings
import extruder.data.Validation
import extruder.laws.EncoderDecoderGenericTests
import extruder.map._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class MapSuite extends FunSuite with Discipline {

  checkAll("Map Monoid", MonoidTests[Map[String, String]].monoid)

  checkAll(
    "Map",
    EncoderDecoderGenericTests[Validation, Settings, Map[String, String], Map[String, String], Map[String, String]](
      defaultSettings
    ).genericEncodeDecode[Int, Int]
  )
}
