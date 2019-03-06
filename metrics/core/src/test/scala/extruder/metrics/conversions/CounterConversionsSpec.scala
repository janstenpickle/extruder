package extruder.metrics.conversions

import extruder.metrics.conversions.counter._
import extruder.metrics.data.CounterValue
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{EitherValues, FunSuite}

class CounterConversionsSpec extends FunSuite with GeneratorDrivenPropertyChecks {
  test("Can convert a numeric into a counter")(forAll { i: Int =>
    val c: CounterValue[Int] = i
    assert(c === CounterValue(i))
  })
}
