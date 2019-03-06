package extruder.metrics.syntax

import extruder.metrics.data.CounterValue
import extruder.metrics.syntax.counter._
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CounterSyntaxSpec extends FunSuite with GeneratorDrivenPropertyChecks {
  test("Can convert a numeric into a gauge")(forAll { i: Int =>
    i.toCounter === CounterValue(i)
  })
}
