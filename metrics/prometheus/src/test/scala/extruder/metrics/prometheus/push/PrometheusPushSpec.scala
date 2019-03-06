package extruder.metrics.prometheus.push

import extruder.metrics.prometheus.TestUtils
import extruder.metrics.snakeCaseTransformation
import io.prometheus.client.Collector
import io.prometheus.client.exporter.PushGateway
import org.mockito.ArgumentCaptor
import org.mockito.Mockito._
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.Matchers._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FunSuite}

import scala.collection.JavaConverters._

class PrometheusPushSpec extends FunSuite with GeneratorDrivenPropertyChecks with MockitoSugar {
  import TestUtils._

  test("Can encode namespaced values")(encodeNamespaced)
  test("Can encode an object")(encodeObject)
  test("Can encode a dimensional object")(encodeDimensionalObject)

  def mockSettings(j: String, ji: String): PrometheusPushMetricSettings = new PrometheusPushMetricSettings {
    override val pushGateway: PushGateway = mock[PushGateway]
    override def jobName: String = j
    override def jobInstance: String = ji

  }

  def encodeNamespaced: Assertion = forAll { (value: Int, name: String, jobName: String, jobInstance: String) =>
    val settings = mockSettings(jobName, jobInstance)

    val collectorCapture: ArgumentCaptor[Collector] = ArgumentCaptor.forClass(classOf[Collector])
    val jobNameCapture: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])

    encode(List(name), settings, value).value.value

    lazy val metric = collectorCapture.getValue.collect().asScala.toList.head
    lazy val sample = metric.samples.asScala.head

    verify(settings.pushGateway).push(collectorCapture.capture(), jobNameCapture.capture())
    assert(jobNameCapture.getValue === jobName)
    assert(metric.name === snakeCaseTransformation(name))
    assert(metric.samples.size === 1)
    (sample.labelNames.asScala should contain).theSameElementsAs(List("metric_type", "instance"))
    (sample.labelValues.asScala should contain).theSameElementsAs(List("gauge", jobInstance))
    assert(sample.value === value.toDouble)
  }

  def encodeObject: Assertion = forAll { (metrics: Metrics, jobName: String, jobInstance: String) =>
    val settings = mockSettings(jobName, jobInstance)

    val collectorCapture: ArgumentCaptor[Collector] = ArgumentCaptor.forClass(classOf[Collector])
    val jobNameCapture: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])

    encode(settings, metrics).value.value

    lazy val capturedMetrics = collectorCapture.getAllValues.asScala.flatMap(_.collect().asScala)
    lazy val samples = capturedMetrics.flatMap(_.samples.asScala)

    verify(settings.pushGateway, times(3)).push(collectorCapture.capture(), jobNameCapture.capture())
    assert(jobNameCapture.getAllValues.asScala === List(jobName, jobName, jobName))
    assert(capturedMetrics.size === 3)
    assert(samples.size === 3)
    (samples.map(_.name) should contain).theSameElementsAs(List("a", "b", "c"))
    (samples.map(_.value) should contain)
      .theSameElementsAs(List(metrics.a.value.toDouble, metrics.b.value.toDouble, metrics.c.value.toDouble))

  }

  def encodeDimensionalObject: Assertion = forAll { (stats: Stats, jobName: String, jobInstance: String) =>
    val settings = mockSettings(jobName, jobInstance)

    val collectorCapture: ArgumentCaptor[Collector] = ArgumentCaptor.forClass(classOf[Collector])
    val jobNameCapture: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])

    encode(settings, stats).value.value

    lazy val capturedMetrics = collectorCapture.getValue.collect().asScala
    lazy val samples = capturedMetrics.flatMap(_.samples.asScala)

    verify(settings.pushGateway).push(collectorCapture.capture(), jobNameCapture.capture())
    assert(jobNameCapture.getValue === jobName)
    assert(capturedMetrics.size === 1)
    assert(samples.size === 3)
    (samples.map(_.name) should contain).theSameElementsAs(List("requests", "requests", "requests"))
    (samples.flatMap(_.labelNames.asScala).distinct should contain)
      .theSameElementsAs(List("metric_type", "instance", "metrics"))
    (samples.flatMap(_.labelValues.asScala).distinct should contain)
      .theSameElementsAs(List("counter", jobInstance, "a", "b", "c").distinct)
    (samples.map(_.value) should contain).theSameElementsAs(
      List(stats.requests.a.value.toDouble, stats.requests.b.value.toDouble, stats.requests.c.value.toDouble)
    )
  }
}
