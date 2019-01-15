package extruder.meta

import extruder.meta.typeable._
import org.scalatest.FunSuite

class ReprTableSuite extends FunSuite {
  test("Can represent a case class as a table") {
    assert(!ReprTable.asTable[CaseClass](List.empty, extruder.map.defaultSettings).isEmpty)
  }
}
