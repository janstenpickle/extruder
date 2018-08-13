package extruder.metrics.spectator

import cats.effect.IO
import com.netflix.spectator.servo.ServoRegistry
import extruder.metrics.data.{CounterValue, GaugeValue}
import extruder.metrics.snakeCaseTransformation
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

import scala.collection.JavaConverters._

class SpectatorRegistrySpec extends Specification with ScalaCheck {
  import SpectatorRegistrySpec._

  override def is: SpecStructure =
    s2"""
        Can encode namespaced values $encodeNamespaced
        Can encode a counter $encodeCounter
        Can encode an object $encodeObject
        Can encode a dimensional object $encodeDimensionalObject
      """

  def encodeNamespaced: Prop = prop { (value: Int, name: String) =>
    val reg = new SpectatorRegistry(new ServoRegistry()).encode[IO, Int](List(name), value).unsafeRunSync()
    val id = reg.createId(snakeCaseTransformation(name))
    val metric = reg.get(id).measure().asScala
    (metric.head.value() === value.toDouble).and(metric.size === 1)
  }

  def encodeCounter: Prop = prop { (value: Int, name: String) =>
    val reg = new SpectatorRegistry(new ServoRegistry())
      .encode[IO, CounterValue[Int]](List(name), CounterValue(value))
      .unsafeRunSync()
    val id = reg.createId(snakeCaseTransformation(name))
    reg.get(id).measure().asScala.size === 1
  }

  def encodeObject: Prop = prop { metrics: Metrics =>
    val reg = new SpectatorRegistry(new ServoRegistry()).encode[IO, Metrics](metrics).unsafeRunSync
    def id(name: String) = reg.createId(name)

    def testMetric(name: String, expected: GaugeValue[Long]) = {
      val metric = reg.get(id(name)).measure().asScala
      (metric.head.value() === expected.value.toDouble).and(metric.size === 1)
    }

    testMetric("a", metrics.a).and(testMetric("b", metrics.b)).and(testMetric("c", metrics.c))
  }

  def encodeDimensionalObject: Prop = prop { stats: Stats =>
    val reg = new SpectatorRegistry(new ServoRegistry()).encode[IO, Stats](stats).unsafeRunSync()

    def id(name: String) = reg.createId("requests").withTags(Map("metrics" -> name).asJava)

    def testMetric(name: String, expected: GaugeValue[Long]) = {
      val metric = reg.get(id(name)).measure().asScala
      (metric.head.value() === expected.value.toDouble).and(metric.size === 1)
    }

    testMetric("a", stats.requests.a)
      .and(testMetric("b", stats.requests.b))
      .and(testMetric("c", stats.requests.c))
  }
}

object SpectatorRegistrySpec {
  case class Metrics(a: GaugeValue[Long], b: GaugeValue[Long], c: GaugeValue[Long])

  case class Stats(requests: Metrics)

  implicit val longArb: Arbitrary[Long] = Arbitrary(Gen.posNum[Long])
  implicit val strArb: Arbitrary[String] = Arbitrary(Gen.alphaStr.map(_.trim).suchThat(_.nonEmpty))
}
