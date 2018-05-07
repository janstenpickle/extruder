package extruder.metrics

import java.util.concurrent.TimeUnit

import cats.Eq
import cats.instances.string._
import cats.kernel.laws.discipline.MonoidTests
import extruder.core.Validation
import extruder.effect.ExtruderMonadError
import extruder.metrics.data._
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.matcher.{EitherMatchers, Matchers}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import shapeless.Coproduct

import scala.concurrent.duration.FiniteDuration

class MetricEncodersSpec
    extends Specification
    with ScalaCheck
    with Matchers
    with EitherMatchers
    with Discipline
    with MetricEncoders {
  import MetricEncodersSpec._

  override type Enc[F[_], T] = TestMetricEncoder[F, T]
  override type OutputData = Metrics
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Hint = TestMetricsHints.type

  private implicit val ev = monoid

  override def is: SpecStructure =
    s2"""
        Can encode an Int $testInt
        Can encode a Long $testLong
        Can encode a Double $testDouble
        Can encode a Float $testFloat
        Can encode a FiniteDuration $testFiniteDuration
        Can encode a non-numeric value as a label $testNonNum

        Can encode a Map $testMap
        Can encode a numeric List $testNumList
        Can encode a non-numeric List $testNonNumList
        Can encode a List of Throwables $testThrowableList

        Can encode metric value $testMetricValue
        Can encode timer value $testTimer
        Can encode values $testValues
        Can encode metric values $testValues

        Can encode an object $testObject
        Can encode an object containing a values map $testObjectMap
        Can encode Both types of object in the same object $testBoth

        ${checkAll("Encoder monoid", MonoidTests[EncodeData].monoid)}
      """

  def testInt(implicit enc: TestMetricEncoder[Validation, Int]): Prop = prop { (i: Int, name: String) =>
    enc.write(List(name), i) must beRight(Map(MetricKey(List(name), None) -> Coproduct[Numbers](i)))
  }

  def testLong(implicit enc: TestMetricEncoder[Validation, Long]): Prop = prop { (l: Long, name: String) =>
    enc.write(List(name), l) must beRight(Map(MetricKey(List(name), None) -> Coproduct[Numbers](l)))
  }

  def testDouble(implicit enc: TestMetricEncoder[Validation, Double]): Prop = prop { (d: Double, name: String) =>
    enc.write(List(name), d) must beRight(Map(MetricKey(List(name), None) -> Coproduct[Numbers](d)))
  }

  def testFloat(implicit enc: TestMetricEncoder[Validation, Float]): Prop = prop { (f: Float, name: String) =>
    enc.write(List(name), f) must beRight(Map(MetricKey(List(name), None) -> Coproduct[Numbers](f)))
  }

  def testMap(implicit enc: TestMetricEncoder[Validation, Map[String, Int]]): Prop = prop { (map: Map[String, Int]) =>
    enc.write(List.empty, map) must beRight(map.map {
      case (k, v) => MetricKey(List(k), None) -> Coproduct[Numbers](v)
    })
  }

  def testFiniteDuration(implicit enc: TestMetricEncoder[Validation, FiniteDuration]): Prop = prop {
    (dur: FiniteDuration, name: String) =>
      enc.write(List(name), dur) must beRight(
        Map(MetricKey(List(name), Some(MetricType.Timer)) -> Coproduct[Numbers](dur.toMillis))
      )
  }

  def testNonNum(implicit enc: TestMetricEncoder[Validation, String]): Prop = prop { (s: String, name: String) =>
    val short: Short = 1
    enc.write(List(name), s) must beRight(
      Map(MetricKey(List(name, s), Some(MetricType.Status)) -> Coproduct[Numbers](short))
    )
  }

  def testNumList(implicit enc: TestMetricEncoder[Validation, List[Int]]): Prop = prop {
    (li: List[Int], name: String) =>
      val key = MetricKey(List(name), None)

      enc.write(List(name), li) must beRight(
        li.foldLeft(Map.empty[MetricKey, Numbers])(
          (acc, v) => acc + (key -> acc.get(key).fold(Coproduct[Numbers](v))(Numbers.add(_, Coproduct[Numbers](v))))
        )
      )
  }

  def testNonNumList(implicit enc: TestMetricEncoder[Validation, List[String]]): Prop = prop {
    (li: List[String], name: String) =>
      val short: Short = 1
      enc.write(List(name), li) must beRight(li.foldLeft(Map.empty[MetricKey, Numbers]) { (acc, v) =>
        val key = MetricKey(List(name, v), Some(MetricType.Status))
        acc + (key -> acc.get(key).fold(Coproduct[Numbers](short))(Numbers.add(_, Coproduct[Numbers](short))))
      })
  }

  def testThrowableList(implicit enc: TestMetricEncoder[Validation, List[Throwable]]): Prop = prop {
    (li: List[Throwable], name: String) =>
      enc.write(List(name), li) must beRight(
        Map(MetricKey(List(name), Some(MetricType.Gauge)) -> Coproduct[Numbers](li.size))
      )
  }

  def testMetricValue(implicit enc: TestMetricEncoder[Validation, MetricValue[Double]]): Prop = prop {
    (mv: MetricValue[Double], name: String) =>
      enc.write(List(name), mv) must beRight(
        Map(MetricKey(List(name), Some(mv.metricType)) -> Coproduct[Numbers](mv.value))
      )
  }

  def testTimer(implicit enc: TestMetricEncoder[Validation, TimerValue[Long]]): Prop = prop {
    (start: Long, finish: Long, name: String) =>
      enc.write(List(name), TimerValue(start, Some(finish))) must
        beRight(Map(MetricKey(List(name), Some(MetricType.Timer)) -> Coproduct[Numbers](finish - start)))
  }

  def testValues(implicit enc: TestMetricEncoder[Validation, MetricValues[MetricValue, Double]]): Prop = prop {
    (values: Map[String, Double]) =>
      enc.write(List.empty, MetricValues(values.mapValues(GaugeValue[Double]))) must beRight(values.map {
        case (k, v) => MetricKey(List(k), Some(MetricType.Gauge)) -> Coproduct[Numbers](v)
      })
  }

  def testObject(implicit enc: TestMetricEncoder[Validation, RequestCount]): Prop = prop { (rq: RequestCount) =>
    enc.write(List.empty, rq) must beRight(
      Map(
        MetricKey(requestCountPath :+ "200", None) -> Coproduct[Numbers](rq.httpRequests.`200`),
        MetricKey(requestCountPath :+ "500", None) -> Coproduct[Numbers](rq.httpRequests.`500`),
        MetricKey(requestCountPath :+ "other", None) -> Coproduct[Numbers](rq.httpRequests.other)
      )
    )
  }

  def testObjectMap(implicit enc: TestMetricEncoder[Validation, HttpRequests]): Prop = prop { (rq: HttpRequests) =>
    enc.write(List.empty, rq) must beRight(rq.statusCode.values.map {
      case (k, v) => MetricKey(httpRequestsPath :+ k, Some(MetricType.Counter)) -> Coproduct[Numbers](v.value)
    })
  }

  def testBoth(implicit enc: TestMetricEncoder[Validation, Both]): Prop = prop { (rq: Both) =>
    enc.write(List.empty, rq) must beRight(
      Map(
        MetricKey(List("Both", "a") ++ requestCountPath :+ "200", None) -> Coproduct[Numbers](rq.a.httpRequests.`200`),
        MetricKey(List("Both", "a") ++ requestCountPath :+ "500", None) -> Coproduct[Numbers](rq.a.httpRequests.`500`),
        MetricKey(List("Both", "a") ++ requestCountPath :+ "other", None) -> Coproduct[Numbers](rq.a.httpRequests.other)
      ) ++ rq.b.statusCode.values.map {
        case (k, v) =>
          MetricKey(List("Both", "b") ++ httpRequestsPath :+ k, Some(MetricType.Counter)) -> Coproduct[Numbers](v.value)
      }
    )
  }

  override protected def mkEncoder[F[_], T](f: (List[String], T) => F[Metrics]): TestMetricEncoder[F, T] =
    new TestMetricEncoder[F, T] {
      override def write(path: List[String], in: T): F[Metrics] = f(path, in)
    }
}

object MetricEncodersSpec {
  val requestCountPath: List[String] = List("RequestCount", "httpRequests", "StatusCode")
  case class StatusCode(`200`: Int, `500`: Int, other: Int)
  case class RequestCount(httpRequests: StatusCode)

  val httpRequestsPath: List[String] = List("HttpRequests", "statusCode")
  case class HttpRequests(statusCode: CounterValues[Int])

  case class Both(a: RequestCount, b: HttpRequests)

  trait TestMetricEncoder[F[_], T] extends MetricEncoder[F, T]

  object TestMetricsHints extends MetricsHints

  implicit val hints: TestMetricsHints.type = TestMetricsHints

  implicit def doubleMetricValueArb: Arbitrary[MetricValue[Double]] =
    Arbitrary(for {
      tpe <- Gen.oneOf(GaugeValue.apply[Double] _, CounterValue.apply[Double] _)
      dbl <- implicitly[Arbitrary[Double]].arbitrary
    } yield tpe(dbl))

  implicit def durArb: Arbitrary[FiniteDuration] =
    Arbitrary(Gen.posNum[Int].map(i => FiniteDuration(i.toLong, TimeUnit.MILLISECONDS)))

  implicit val metricTypeArb: Arbitrary[MetricType] = Arbitrary(
    Gen.oneOf(MetricType.Counter, MetricType.Gauge, MetricType.Status, MetricType.Timer)
  )

  implicit val metricsEq: Eq[Metrics] = Eq.by(_.mapValues(Numbers.toDouble).toString)
}
