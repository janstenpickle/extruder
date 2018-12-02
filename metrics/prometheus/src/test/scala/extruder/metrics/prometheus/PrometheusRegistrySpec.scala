package extruder.metrics.prometheus

import cats.effect.IO
import extruder.metrics.snakeCaseTransformation
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FunSuite}

class PrometheusRegistrySpec extends FunSuite with GeneratorDrivenPropertyChecks {
  import TestUtils._

  test("Can encode namespaced values")(encodeNamespaced)
  test("Can encode an object")(encodeObject)
  test("Can encode a dimensional object")(encodeDimensionalObject)

  case class X(a: String, b: Int)

  def encodeNamespaced: Assertion = forAll { (value: Int, name: String) =>
    val reg = new PrometheusRegistry().encodeF[IO](List(name), value).unsafeRunSync()
    assert(
      reg
        .getSampleValue(snakeCaseTransformation(name), Array("metric_type"), Array("gauge")) === value.toDouble
    )
  }

  def encodeObject: Assertion = forAll { metrics: Metrics =>
    val reg = new PrometheusRegistry().encodeF[IO](metrics).unsafeRunSync
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
    val reg = new PrometheusRegistry().encodeF[IO](stats).unsafeRunSync()
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
