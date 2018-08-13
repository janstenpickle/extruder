package extruder.metrics.prometheus

import cats.effect.IO
import extruder.metrics.snakeCaseTransformation
import io.prometheus.client.Collector
import io.prometheus.client.exporter.PushGateway
import org.scalacheck.Prop
import org.scalacheck.ScalacheckShapeless._
import org.specs2.mock.Mockito
import org.specs2.mock.mockito.ArgumentCapture
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

import scala.collection.JavaConverters._

class PrometheusPushSpec extends Specification with ScalaCheck with Mockito {
  import TestUtils._

  override def is: SpecStructure =
    s2"""
        Can encode namespaced values $encodeNamespaced
        Can encode an object $encodeObject
        Can encode a dimensional object $encodeDimensionalObject
      """

  def encodeNamespaced: Prop = prop { (value: Int, name: String, jobName: String, jobInstance: String) =>
    val push = mock[PushGateway]

    val collectorCapture = new ArgumentCapture[Collector]
    val jobNameCapture = new ArgumentCapture[String]

    PrometheusPush(push, jobName, jobInstance)
      .encode[IO, Int](List(name), value)
      .unsafeRunSync()

    lazy val metric = collectorCapture.value.collect().asScala.toList.head
    lazy val sample = metric.samples.asScala.head

    there
      .was(one(push).push(collectorCapture, jobNameCapture))
      .and(jobNameCapture.value === jobName)
      .and(metric.name === snakeCaseTransformation(name))
      .and(metric.samples.size === 1)
      .and(sample.labelNames.asScala must containTheSameElementsAs(List("instance")))
      .and(sample.labelValues.asScala must containTheSameElementsAs(List(jobInstance)))
      .and(sample.value === value.toDouble)
  }

  def encodeObject: Prop = prop { (metrics: Metrics, jobName: String, jobInstance: String) =>
    val push = mock[PushGateway]

    val collectorCapture = new ArgumentCapture[Collector]
    val jobNameCapture = new ArgumentCapture[String]

    PrometheusPush(push, jobName, jobInstance)
      .encode[IO, Metrics](metrics)
      .unsafeRunSync()

    lazy val capturedMetrics = collectorCapture.values.asScala.flatMap(_.collect().asScala)
    lazy val samples = capturedMetrics.flatMap(_.samples.asScala)

    there
      .was(three(push).push(collectorCapture, jobNameCapture))
      .and(jobNameCapture.values.asScala === List(jobName, jobName, jobName))
      .and(capturedMetrics.size === 3)
      .and(samples.size === 3)
      .and(samples.map(_.name) must containTheSameElementsAs(List("a", "b", "c")))
      .and(
        samples.map(_.value) must containTheSameElementsAs(
          List(metrics.a.value.toDouble, metrics.b.value.toDouble, metrics.c.value.toDouble)
        )
      )
  }

  def encodeDimensionalObject: Prop = prop { (stats: Stats, jobName: String, jobInstance: String) =>
    val push = mock[PushGateway]

    val collectorCapture = new ArgumentCapture[Collector]
    val jobNameCapture = new ArgumentCapture[String]

    PrometheusPush(push, jobName, jobInstance)
      .encode[IO, Stats](stats)
      .unsafeRunSync()

    lazy val capturedMetrics = collectorCapture.value.collect().asScala
    lazy val samples = capturedMetrics.flatMap(_.samples.asScala)

    there
      .was(one(push).push(collectorCapture, jobNameCapture))
      .and(jobNameCapture.value === jobName)
      .and(capturedMetrics.size === 1)
      .and(samples.size === 3)
      .and(samples.map(_.name) must containTheSameElementsAs(List("requests", "requests", "requests")))
      .and(samples.flatMap(_.labelNames.asScala).distinct must containTheSameElementsAs(List("instance", "metrics")))
      .and(
        samples.flatMap(_.labelValues.asScala).distinct must containTheSameElementsAs(
          List(jobInstance, "a", "b", "c").distinct
        )
      )
      .and(
        samples.map(_.value) must containTheSameElementsAs(
          List(stats.requests.a.value.toDouble, stats.requests.b.value.toDouble, stats.requests.c.value.toDouble)
        )
      )
  }
}
