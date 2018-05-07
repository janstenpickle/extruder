package extruder.metrics.conversions

import extruder.metrics.conversions.counter._
import extruder.metrics.data.CounterValue
import org.scalacheck.Prop
import org.specs2.matcher.Matchers
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class CounterConversionsSpec extends Specification with ScalaCheck with Matchers {
  override def is: SpecStructure =
    s2"""
        Can convert a numeric into a counter $testNumeric
      """

  def testNumeric: Prop = prop { (i: Int) =>
    val c: CounterValue[Int] = i
    c === CounterValue(i)
  }
}
