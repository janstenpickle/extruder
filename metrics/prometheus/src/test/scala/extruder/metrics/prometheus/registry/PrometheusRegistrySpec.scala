package extruder.metrics.prometheus.registry

import extruder.metrics.prometheus.TestUtils
import extruder.metrics.snakeCaseTransformation
import io.prometheus.client.CollectorRegistry
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite}

class PrometheusRegistrySpec extends FunSuite with GeneratorDrivenPropertyChecks with EitherValues {
  import TestUtils._

  test("Can encode namespaced values")(encodeNamespaced)
  test("Can encode an object")(encodeObject)
  test("Can encode a dimensional object")(encodeDimensionalObject)

  case class X(a: String, b: Int)

  def settings = new PrometheusRegistryMetricSettings {
    override val registry: CollectorRegistry = new CollectorRegistry()
  }

  def encodeNamespaced: Assertion = forAll { (value: Int, name: String) =>
    val reg: CollectorRegistry = encode(List(name), settings, value).value.value.right.value
    assert(
      reg
        .getSampleValue(snakeCaseTransformation(name), Array("metric_type"), Array("gauge")) === value.toDouble
    )
  }

  def encodeObject: Assertion = forAll { metrics: Metrics =>
    val reg: CollectorRegistry = encode(settings, metrics).value.value.right.value
    assert(
      reg
        .getSampleValue(snakeCaseTransformation("a"), Array("metric_type"), Array("counter")) === metrics.a.value.toDouble
    )
    assert(
      reg.getSampleValue(snakeCaseTransformation("b"), Array("metric_type"), Array("counter")) === metrics.b.value.toDouble
    )
    assert(
      reg.getSampleValue(snakeCaseTransformation("c"), Array("metric_type"), Array("counter")) === metrics.c.value.toDouble
    )
  }

  def encodeDimensionalObject: Assertion = forAll { stats: Stats =>
    val reg: CollectorRegistry = encode(settings, stats).value.value.right.value
    assert(
      reg
        .getSampleValue(snakeCaseTransformation("requests"), Array("metric_type", "metrics"), Array("counter", "a")) === stats.requests.a.value.toDouble
    )
    assert(
      reg
        .getSampleValue(snakeCaseTransformation("requests"), Array("metric_type", "metrics"), Array("counter", "b")) === stats.requests.b.value.toDouble
    )
    assert(
      reg
        .getSampleValue(snakeCaseTransformation("requests"), Array("metric_type", "metrics"), Array("counter", "c")) === stats.requests.c.value.toDouble
    )

  }
}
