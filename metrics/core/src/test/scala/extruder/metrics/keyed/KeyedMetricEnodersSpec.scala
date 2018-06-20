package extruder.metrics.keyed

import extruder.core.{Encode, Validation}
import extruder.effect.ExtruderMonadError
import extruder.metrics.MetricSettings
import extruder.metrics.data.{MetricType, Metrics, Numbers}
import org.scalacheck.Prop
import org.scalacheck.ScalacheckShapeless._
import org.specs2.matcher.{EitherMatchers, Matchers}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import shapeless.Coproduct

class KeyedMetricEnodersSpec
    extends Specification
    with ScalaCheck
    with Matchers
    with EitherMatchers
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

  override def is: SpecStructure =
    s2"""
        Can encode an object $testObject
      """

  def testObject: Prop = prop { (req: RequestCount) =>
    encode[RequestCount](req) must beRight.which {
      _ must containTheSameElementsAs(
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
      )
    }
  }

}
