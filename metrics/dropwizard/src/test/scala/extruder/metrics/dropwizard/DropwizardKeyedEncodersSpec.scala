package extruder.metrics.dropwizard

import cats.effect.IO
import extruder.metrics._
import extruder.metrics.data.{CounterValue, GaugeValue, TimerValue}
import io.dropwizard.metrics5.MetricName
import org.scalacheck.Prop
import org.scalacheck.ScalacheckShapeless._
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class DropwizardKeyedEncodersSpec extends Specification with ScalaCheck {
  import DropwizardKeyedEncodersSpec._

  override def is: SpecStructure =
    s2"""
        Can encode namespaced values $encodeNamespaced
        Can encode an object $encodeObject
      """

  def encodeNamespaced: Prop = prop { (value: Int, name: String) =>
    val reg = new DropwizardKeyedRegistry().encode[IO, Int](List(name), value).unsafeRunSync()
    (reg.getGauges.size() === 1)
      .and(reg.getGauges().get(MetricName.build(snakeCaseTransformation(name))).getValue === value.toDouble)
  }

  def encodeObject: Prop = prop { metrics: Metrics =>
    val reg = new DropwizardKeyedRegistry().encode[IO, Metrics](metrics).unsafeRunSync()
    (reg.getGauges.size() === 2)
      .and(reg.getCounters.size() === 1)
      .and(reg.getGauges.get(MetricName.build("metrics.timer")).getValue === metrics.timer.value.toDouble)
      .and(reg.getGauges.get(MetricName.build("metrics.gauge")).getValue === metrics.gauge.value.toDouble)
      .and(reg.getCounters.get(MetricName.build("metrics.counter")).getCount === metrics.counter.value)
  }
}

object DropwizardKeyedEncodersSpec {
  case class Metrics(counter: CounterValue[Long], timer: TimerValue[Int], gauge: GaugeValue[Float])
}
