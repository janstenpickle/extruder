package extruder.metrics.dropwizard

import cats.effect.IO
import extruder.metrics.data.CounterValue
import extruder.metrics.snakeCaseTransformation
import io.dropwizard.metrics5.MetricName
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FunSuite}

class DropwizardDimensionalEncodersSpec extends FunSuite with GeneratorDrivenPropertyChecks {
  import DropwizardDimensionalEncodersSpec._

  test("Can encode namespaced values")(encodeNamespaced)
  test("Can encode an object")(encodeObject)
  test("Can encode a dimensional object")(encodeDimensionalObject)

  def encodeNamespaced: Assertion = forAll { (value: Int, name: String) =>
    val reg = new DropwizardDimensionalRegistry().encode[IO, Int](List(name), value).unsafeRunSync()
    val metricName = MetricName.build(snakeCaseTransformation(name)).tagged("metric_type", "gauge")
    assert(
      reg
        .getGauges()
        .get(metricName)
        .getValue === value.toDouble
    )
  }

  def encodeObject: Assertion = forAll { metrics: Metrics =>
    val reg = new DropwizardDimensionalRegistry().encode[IO, Metrics](metrics).unsafeRunSync
    def metricName(name: String) = MetricName.build(snakeCaseTransformation(name)).tagged("metric_type", "counter")
    assert(reg.getCounters().get(metricName("a")).getCount === metrics.a.value)
    assert(reg.getCounters().get(metricName("b")).getCount === metrics.b.value)
    assert(reg.getCounters().get(metricName("c")).getCount === metrics.c.value)
  }

  def encodeDimensionalObject: Assertion = forAll { stats: Stats =>
    val reg = new DropwizardDimensionalRegistry().encode[IO, Stats](stats).unsafeRunSync
    def metricName(name: String) =
      MetricName.build(snakeCaseTransformation("requests")).tagged("metric_type", "counter", "metrics", name)
    assert(reg.getCounters().get(metricName("a")).getCount === stats.requests.a.value)
    assert(reg.getCounters().get(metricName("b")).getCount === stats.requests.b.value)
    assert(reg.getCounters().get(metricName("c")).getCount === stats.requests.c.value)
  }
}

object DropwizardDimensionalEncodersSpec {
  case class Metrics(a: CounterValue[Long], b: CounterValue[Long], c: CounterValue[Long])

  case class Stats(requests: Metrics)

  implicit val longArb: Arbitrary[Long] = Arbitrary(Gen.posNum[Long])
  implicit val strArb: Arbitrary[String] = Arbitrary(Gen.alphaStr.map(_.trim).suchThat(_.nonEmpty))
}
