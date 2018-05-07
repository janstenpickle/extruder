package extruder.metrics.conversions

import extruder.metrics.conversions.gauge._
import extruder.metrics.data.GaugeValue
import org.scalacheck.Prop
import org.specs2.matcher.Matchers
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class GaugeConversionsSpec extends Specification with ScalaCheck with Matchers {
  override def is: SpecStructure =
    s2"""
        Can convert a numeric into a gauge $testNumeric
      """

  def testNumeric: Prop = prop { (i: Int) =>
    val c: GaugeValue[Int] = i
    c === GaugeValue(i)
  }
}
