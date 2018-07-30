package extruder.metrics.keyed

import extruder.core.Encode
import extruder.effect.ExtruderMonadError
import extruder.metrics.MetricSettings
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{EitherValues, FunSuite}
import shapeless.Coproduct

class KeyedMetricEnodersSpec
    extends FunSuite
    with GeneratorDrivenPropertyChecks
    with EitherValues
    with KeyedMetricEncoders
    with Encode {
  import extruder.metrics.MetricEncodersSpec._

  override type Enc[F[_], T] = TestMetricEncoder[F, T]

  override type OutputData = Iterable[KeyedMetric]
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Sett = MetricSettings

  override def defaultSettings: MetricSettings = new MetricSettings {}

  override protected def mkEncoder[F[_], T](f: (List[String], Sett, T) => F[Metrics]): TestMetricEncoder[F, T] =
    new TestMetricEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Metrics] = f(path, settings, in)
    }

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: Metrics)(
    implicit F: ExtruderMonadError[F]
  ): F[Iterable[KeyedMetric]] = buildMetrics(settings, inter, MetricType.Gauge)

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
