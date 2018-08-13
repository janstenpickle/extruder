package extruder.metrics.dropwizard

import cats.effect.IO
import extruder.metrics.data.CounterValue
import extruder.metrics.snakeCaseTransformation
import io.dropwizard.metrics5.MetricName
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.ScalacheckShapeless._
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class DropwizardDimensionalEncodersSpec extends Specification with ScalaCheck {
  import DropwizardDimensionalEncodersSpec._

  override def is: SpecStructure =
    s2"""
        Can encode namespaced values $encodeNamespaced
        Can encode an object $encodeObject
        Can encode a dimensional object $encodeDimensionalObject
      """

  def encodeNamespaced: Prop = prop { (value: Int, name: String) =>
    val reg = new DropwizardDimensionalRegistry().encode[IO, Int](List(name), value).unsafeRunSync()
    val metricName = MetricName.build(snakeCaseTransformation(name))
    reg
      .getGauges()
      .get(metricName)
      .getValue === value.toDouble
  }

  def encodeObject: Prop = prop { metrics: Metrics =>
    val reg = new DropwizardDimensionalRegistry().encode[IO, Metrics](metrics).unsafeRunSync
    def metricName(name: String) = MetricName.build(snakeCaseTransformation(name))
    (reg.getCounters().get(metricName("a")).getCount === metrics.a.value)
      .and(reg.getCounters().get(metricName("b")).getCount === metrics.b.value)
      .and(reg.getCounters().get(metricName("c")).getCount === metrics.c.value)
  }

  def encodeDimensionalObject: Prop = prop { stats: Stats =>
    val reg = new DropwizardDimensionalRegistry().encode[IO, Stats](stats).unsafeRunSync
    def metricName(name: String) =
      MetricName.build(snakeCaseTransformation("requests")).tagged("metrics", name)
    (reg.getCounters().get(metricName("a")).getCount === stats.requests.a.value)
      .and(reg.getCounters().get(metricName("b")).getCount === stats.requests.b.value)
      .and(reg.getCounters().get(metricName("c")).getCount === stats.requests.c.value)

  }
}

object DropwizardDimensionalEncodersSpec {
  case class Metrics(a: CounterValue[Long], b: CounterValue[Long], c: CounterValue[Long])

  case class Stats(requests: Metrics)

  implicit val longArb: Arbitrary[Long] = Arbitrary(Gen.posNum[Long])
  implicit val strArb: Arbitrary[String] = Arbitrary(Gen.alphaStr.map(_.trim).suchThat(_.nonEmpty))
}
