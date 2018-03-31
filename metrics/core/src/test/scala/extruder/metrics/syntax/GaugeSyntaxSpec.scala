package extruder.metrics.syntax

import extruder.metrics.data.GaugeValue
import extruder.metrics.syntax.gauge._
import org.scalacheck.Prop
import org.specs2.matcher.Matchers
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class GaugeSyntaxSpec extends Specification with ScalaCheck with Matchers {
  override def is: SpecStructure =
    s2"""
        Can convert a numeric into a gauge $testNumeric
      """

  def testNumeric: Prop = prop { (i: Int) =>
    i.toGauge === GaugeValue(i)
  }
}
