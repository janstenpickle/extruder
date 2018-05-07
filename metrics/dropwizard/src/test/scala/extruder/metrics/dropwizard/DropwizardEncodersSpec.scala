package extruder.metrics.dropwizard

import cats.effect.IO
import extruder.metrics._
import extruder.metrics.data.{CounterValue, GaugeValue, TimerValue}
import org.scalacheck.Prop
import org.scalacheck.ScalacheckShapeless._
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class DropwizardEncodersSpec extends Specification with ScalaCheck {
  import DropwizardEncodersSpec._

  override def is: SpecStructure =
    s2"""
        Can encode namespaced values $encodeNamespaced
        Can encode an object $encodeObject
      """

  def encodeNamespaced: Prop = prop { (value: Int, name: String) =>
    val reg = new DropwizardRegistry().encode[Int, IO](List(name), value).unsafeRunSync()
    (reg.getGauges.size() === 1).and(reg.getGauges().get(snakeCaseTransformation(name)).getValue === value.toDouble)
  }

  def encodeObject: Prop = prop { metrics: Metrics =>
    val reg = new DropwizardRegistry().encode[Metrics, IO](metrics).unsafeRunSync()
    (reg.getGauges.size() === 2)
      .and(reg.getCounters.size() === 1)
      .and(reg.getGauges.get("metrics.timer").getValue === metrics.timer.value.toDouble)
      .and(reg.getGauges.get("metrics.gauge").getValue === metrics.gauge.value.toDouble)
      .and(reg.getCounters.get("metrics.counter").getCount === metrics.counter.value)
  }
}

object DropwizardEncodersSpec {
  case class Metrics(counter: CounterValue[Long], timer: TimerValue[Int], gauge: GaugeValue[Float])
}
