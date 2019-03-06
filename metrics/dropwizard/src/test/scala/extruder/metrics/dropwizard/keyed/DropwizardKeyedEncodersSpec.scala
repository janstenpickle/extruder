package extruder.metrics.dropwizard.keyed

import extruder.metrics._
import extruder.metrics.data.{CounterValue, GaugeValue, TimerValue}
import io.dropwizard.metrics5.{MetricName, MetricRegistry}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite}

class DropwizardKeyedEncodersSpec extends FunSuite with GeneratorDrivenPropertyChecks with EitherValues {
  import DropwizardKeyedEncodersSpec._

  test("Can encode namespaced values")(encodeNamespaced)
  test("Can encode an object")(encodeObject)

  def settings: DropwizardKeyedMetricSettings = new DropwizardKeyedMetricSettings {}

  def encodeNamespaced: Assertion = forAll { (value: Int, name: String) =>
    val reg: MetricRegistry = encode(List(name), settings, value).value.value.right.value
    assert(reg.getGauges.size() === 1)
    assert(reg.getGauges().get(MetricName.build(snakeCaseTransformation(name))).getValue === value.toDouble)
  }

  def encodeObject: Assertion = forAll { metrics: Metrics =>
    val reg: MetricRegistry = encode(settings, metrics).value.value.right.value
    assert(reg.getGauges.size() === 2)
    assert(reg.getCounters.size() === 1)
    assert(reg.getGauges.get(MetricName.build("metrics.timer")).getValue === metrics.timer.value.toDouble)
    assert(reg.getGauges.get(MetricName.build("metrics.gauge")).getValue === metrics.gauge.value.toDouble)
    assert(reg.getCounters.get(MetricName.build("metrics.counter")).getCount === metrics.counter.value)
  }
}

object DropwizardKeyedEncodersSpec {
  case class Metrics(counter: CounterValue[Long], timer: TimerValue[Int], gauge: GaugeValue[Float])
}
