package extruder.metrics.keyed

import extruder.core.{DataSource, Encode}
import extruder.data.Validation
import extruder.metrics.MetricSettings
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import shapeless.Coproduct

class KeyedMetricEncodersSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with EitherValues {
  import KeyedMetricEncodersSpec.TestKeyedMetricEncoders._
  import extruder.metrics.MetricEncodersSpec._

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
