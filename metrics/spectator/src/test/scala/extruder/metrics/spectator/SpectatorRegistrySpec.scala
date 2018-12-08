package extruder.metrics.spectator

import com.netflix.spectator.api.Registry
import com.netflix.spectator.servo.ServoRegistry
import extruder.cats.effect.EvalValidation
import extruder.metrics.data.{CounterValue, GaugeValue}
import extruder.metrics.snakeCaseTransformation
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite}

import scala.collection.JavaConverters._

class SpectatorRegistrySpec extends FunSuite with GeneratorDrivenPropertyChecks with EitherValues {
  import SpectatorRegistrySpec._

  test("Can encode namespaced values")(encodeNamespaced)
  test("Can encode a counter")(encodeCounter)
  test("Can encode an object")(encodeObject)
  test("Can encode a dimensional object")(encodeDimensionalObject)

  def settings: SpectatorMetricSettings = new SpectatorMetricSettings {
    override val registry: Registry = new ServoRegistry()
  }
  spectatorFinalizer[EvalValidation, SpectatorMetricSettings]

  def encodeNamespaced: Assertion = forAll { (value: Int, name: String) =>
    val reg: Registry = encode(List(name), settings, value).value.value.right.value
    val id = reg.createId(snakeCaseTransformation(name)).withTags(Map("metric_type" -> "gauge").asJava)
    val metric = reg.get(id).measure().asScala
    assert(metric.head.value() === value.toDouble)
    assert(metric.size === 1)
  }

  def encodeCounter: Assertion = forAll { (value: Int, name: String) =>
    val reg: Registry = encode(List(name), settings, CounterValue(value)).value.value.right.value
    val id = reg.createId(snakeCaseTransformation(name)).withTags(Map("metric_type" -> "counter").asJava)
    assert(reg.get(id).measure().asScala.size === 1)
  }

  def encodeObject: Assertion = forAll { metrics: Metrics =>
    val reg: Registry = encode(settings, metrics).value.value.right.value
    def id(name: String) = reg.createId(name).withTags(Map("metric_type" -> "gauge").asJava)

    def testMetric(name: String, expected: GaugeValue[Long]) = {
      val metric = reg.get(id(name)).measure().asScala
      assert(metric.head.value() === expected.value.toDouble)
      assert(metric.size === 1)
    }

    testMetric("a", metrics.a)
    testMetric("b", metrics.b)
    testMetric("c", metrics.c)
  }

  def encodeDimensionalObject: Assertion = forAll { stats: Stats =>
    val reg: Registry = encode(settings, stats).value.value.right.value

    def id(name: String) = reg.createId("requests").withTags(Map("metric_type" -> "gauge", "metrics" -> name).asJava)

    def testMetric(name: String, expected: GaugeValue[Long]) = {
      val metric = reg.get(id(name)).measure().asScala
      assert(metric.head.value() === expected.value.toDouble)
      assert(metric.size === 1)
    }

    testMetric("a", stats.requests.a)
    testMetric("b", stats.requests.b)
    testMetric("c", stats.requests.c)
  }
}

object SpectatorRegistrySpec {
  case class Metrics(a: GaugeValue[Long], b: GaugeValue[Long], c: GaugeValue[Long])

  case class Stats(requests: Metrics)

  implicit val longArb: Arbitrary[Long] = Arbitrary(Gen.posNum[Long])
  implicit val strArb: Arbitrary[String] = Arbitrary(Gen.alphaStr.map(_.trim).suchThat(_.nonEmpty))
}
