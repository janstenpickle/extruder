package extruder.metrics.keyed

import extruder.core.{DataSource, Encode}
import extruder.data.Validation
import extruder.metrics.MetricSettings
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{EitherValues, FunSuite}
import shapeless.Coproduct

class KeyedMetricEncodersSpec extends FunSuite with GeneratorDrivenPropertyChecks with EitherValues {
  import extruder.metrics.MetricEncodersSpec._
  import KeyedMetricEncodersSpec.TestKeyedMetricEncoders._

  test("Can encode an object")(forAll { req: RequestCount =>
    encode[RequestCount](req).right.value ===
      List(
        KeyedMetric(
          "request_count.http_requests.status_code.200",
          MetricType.Gauge,
          Coproduct[Numbers](req.httpRequests.`200`)
        ),
        KeyedMetric(
          "request_count.http_requests.status_code.500",
          MetricType.Gauge,
          Coproduct[Numbers](req.httpRequests.`500`)
        ),
        KeyedMetric(
          "request_count.http_requests.status_code.other",
          MetricType.Gauge,
          Coproduct[Numbers](req.httpRequests.other)
        )
      )
  })

}

object KeyedMetricEncodersSpec {
  object TestKeyedMetricEncoders extends KeyedMetricEncoderInstances with Encode with DataSource {
    override type EncodeData = Metrics
    override type OutputData = Iterable[KeyedMetric]
    override type EncodeDefault[A] = Validation[A]
    override type Sett = MetricSettings
    override def defaultSettings: MetricSettings = new MetricSettings {}
  }
}
