package extruder.metrics

import java.util.concurrent.TimeUnit

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.map._
import cats.instances.long._
import cats.kernel.laws.discipline.MonoidTests
import extruder.core.Validation
import extruder.effect.ExtruderMonadError
import extruder.metrics.data.MetricType.Counter
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
  override type Sett = MetricSettings

  override def defaultSettings: MetricSettings = new MetricSettings {}

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

        Can encode single dimensional map data $testSingleDimensional
        Can encode multi dimensional map data $testMultiDimensional
        Can encode object dimensional map data $testObjectDimensional

        Can encode single dimensional counter values $testSingleDimensionalValues
        Can encode multi dimensional counter values $testMultiDimensionalValues
        Can encode object dimensional counter values $testObjectDimensionalValues

        ${checkAll("Encoder monoid", MonoidTests[EncodeData].monoid)}
      """

  def testInt(implicit enc: TestMetricEncoder[Validation, Int]): Prop = prop { (i: Int, name: String) =>
    enc.write(List(name), defaultSettings, i) must beRight(
      Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](i))
    )
  }

  def testLong(implicit enc: TestMetricEncoder[Validation, Long]): Prop = prop { (l: Long, name: String) =>
    enc.write(List(name), defaultSettings, l) must beRight(
      Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](l))
    )
  }

  def testDouble(implicit enc: TestMetricEncoder[Validation, Double]): Prop = prop { (d: Double, name: String) =>
    enc.write(List(name), defaultSettings, d) must beRight(
      Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](d))
    )
  }

  def testFloat(implicit enc: TestMetricEncoder[Validation, Float]): Prop = prop { (f: Float, name: String) =>
    enc.write(List(name), defaultSettings, f) must beRight(
      Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](f))
    )
  }

  def testMap(implicit enc: TestMetricEncoder[Validation, Map[String, Int]]): Prop = prop { map: Map[String, Int] =>
    enc.write(List.empty, defaultSettings, map) must beRight(map.map {
      case (k, v) => SimpleMetricKey(k, List.empty, None) -> Coproduct[Numbers](v)
    })
  }

  def testFiniteDuration(implicit enc: TestMetricEncoder[Validation, FiniteDuration]): Prop = prop {
    (dur: FiniteDuration, name: String) =>
      enc.write(List(name), defaultSettings, dur) must beRight(
        Map(SimpleMetricKey(name, List.empty, Some(MetricType.Timer)) -> Coproduct[Numbers](dur.toMillis))
      )
  }

  def testNonNum(implicit enc: TestMetricEncoder[Validation, String]): Prop = prop { (s: String, name: String) =>
    val short: Short = 1
    enc.write(List(name), defaultSettings, s) must beRight(
      Map(SimpleMetricKey(s, List(name), Some(MetricType.Status)) -> Coproduct[Numbers](short))
    )
  }

  def testNumList(implicit enc: TestMetricEncoder[Validation, List[Int]]): Prop = prop {
    (li: List[Int], name: String) =>
      val key = SimpleMetricKey(name, List.empty, None)

      enc.write(List(name), defaultSettings, li) must beRight(
        li.foldLeft(Map.empty[MetricKey, Numbers])(
          (acc, v) => acc + (key -> acc.get(key).fold(Coproduct[Numbers](v))(Numbers.add(_, Coproduct[Numbers](v))))
        )
      )
  }

  def testNonNumList(implicit enc: TestMetricEncoder[Validation, List[String]]): Prop = prop {
    (li: List[String], name: String) =>
      val short: Short = 1
      enc.write(List(name), defaultSettings, li) must beRight(li.foldLeft(Map.empty[MetricKey, Numbers]) { (acc, v) =>
        val key = SimpleMetricKey(v, List(name), Some(MetricType.Status))
        acc + (key -> acc.get(key).fold(Coproduct[Numbers](short))(Numbers.add(_, Coproduct[Numbers](short))))
      })
  }

  def testThrowableList(implicit enc: TestMetricEncoder[Validation, List[Throwable]]): Prop = prop {
    (li: List[Throwable], name: String) =>
      enc.write(List(name), defaultSettings, li) must beRight(
        Map(SimpleMetricKey(name, List.empty, Some(MetricType.Gauge)) -> Coproduct[Numbers](li.size))
      )
  }

  def testMetricValue(implicit enc: TestMetricEncoder[Validation, MetricValue[Double]]): Prop = prop {
    (mv: MetricValue[Double], name: String) =>
      enc.write(List(name), defaultSettings, mv) must beRight(
        Map(SimpleMetricKey(name, List.empty, Some(mv.metricType)) -> Coproduct[Numbers](mv.value))
      )
  }

  def testTimer(implicit enc: TestMetricEncoder[Validation, TimerValue[Long]]): Prop = prop {
    (start: Long, finish: Long, name: String) =>
      enc.write(List(name), defaultSettings, TimerValue(start, Some(finish))) must
        beRight(Map(SimpleMetricKey(name, List.empty, Some(MetricType.Timer)) -> Coproduct[Numbers](finish - start)))
  }

  def testValues(implicit enc: TestMetricEncoder[Validation, MetricValues[MetricValue, String, Double]]): Prop = prop {
    values: Map[String, Double] =>
      enc.write(List.empty, defaultSettings, MetricValues(values.mapValues(GaugeValue[Double]))) must beRight(
        values.map {
          case (k, v) => SimpleMetricKey(k, List.empty, Some(MetricType.Gauge)) -> Coproduct[Numbers](v)
        }
      )
  }

  def testObject(implicit enc: TestMetricEncoder[Validation, RequestCount]): Prop = prop { rq: RequestCount =>
    enc.write(List.empty, defaultSettings, rq) must beRight(
      Map(
        DimensionalMetricKey("httpRequests", List("RequestCount"), Map("StatusCode" -> "200"), None) -> Coproduct[
          Numbers
        ](rq.httpRequests.`200`),
        DimensionalMetricKey("httpRequests", List("RequestCount"), Map("StatusCode" -> "500"), None) -> Coproduct[
          Numbers
        ](rq.httpRequests.`500`),
        DimensionalMetricKey("httpRequests", List("RequestCount"), Map("StatusCode" -> "other"), None) -> Coproduct[
          Numbers
        ](rq.httpRequests.other)
      )
    )
  }

  def testObjectMap(implicit enc: TestMetricEncoder[Validation, HttpRequests]): Prop = prop { rq: HttpRequests =>
    enc.write(List.empty, defaultSettings, rq) must beRight(rq.statusCode.values.map {
      case (k, v) =>
        DimensionalMetricKey("HttpRequests", List.empty, Map("statusCode" -> k), Some(MetricType.Counter)) -> Coproduct[
          Numbers
        ](v.value)
    })
  }

  def testBoth(implicit enc: TestMetricEncoder[Validation, Both]): Prop = prop { rq: Both =>
    enc.write(List.empty, defaultSettings, rq) must beRight(
      Map(
        DimensionalMetricKey("httpRequests", List("Both", "a", "RequestCount"), Map("StatusCode" -> "200"), None) -> Coproduct[
          Numbers
        ](rq.a.httpRequests.`200`),
        DimensionalMetricKey("httpRequests", List("Both", "a", "RequestCount"), Map("StatusCode" -> "500"), None) -> Coproduct[
          Numbers
        ](rq.a.httpRequests.`500`),
        DimensionalMetricKey("httpRequests", List("Both", "a", "RequestCount"), Map("StatusCode" -> "other"), None) -> Coproduct[
          Numbers
        ](rq.a.httpRequests.other)
      ) ++ rq.b.statusCode.values.map {
        case (k, v) =>
          DimensionalMetricKey("HttpRequests", List("Both", "b"), Map("statusCode" -> k), Some(MetricType.Counter)) -> Coproduct[
            Numbers
          ](v.value)
      }
    )
  }

  def testSingleDimensional(implicit enc: TestMetricEncoder[Validation, Map[(Short, Int), Int]]): Prop = prop {
    data: Map[(Short, Int), Int] =>
      enc.write(List("data"), defaultSettings, data) must beRight(data.map {
        case ((labelName, labelValue), v) =>
          DimensionalMetricKey("data", List.empty, Map(labelName.toString -> labelValue.toString), None) -> Coproduct[
            Numbers
          ](v)
      })
  }

  def testMultiDimensional(implicit enc: TestMetricEncoder[Validation, Map[Map[Short, Boolean], Int]]): Prop = prop {
    data: Map[Map[Short, Boolean], Int] =>
      enc.write(List("data"), defaultSettings, data) must beRight(data.map {
        case (labels, v) =>
          DimensionalMetricKey("data", List.empty, labels.map {
            case (labelName, labelValue) => labelName.toString -> labelValue.toString
          }, None) -> Coproduct[Numbers](v)
      })
  }

  def testObjectDimensional(implicit enc: TestMetricEncoder[Validation, Map[Dimensions, Int]]): Prop = prop {
    data: Map[Dimensions, Int] =>
      enc.write(List("data"), defaultSettings, data) must beRight(data.map {
        case (dims, v) =>
          DimensionalMetricKey(
            "data",
            List.empty,
            Map("dimensionOne" -> dims.dimensionOne.toString, "dimensionTwo" -> dims.dimensionTwo.toString),
            None
          ) -> Coproduct[Numbers](v)
      })
  }

  def testSingleDimensionalValues(implicit enc: TestMetricEncoder[Validation, CounterValues[(Short, Int), Int]]): Prop =
    prop { data: CounterValues[(Short, Int), Int] =>
      enc.write(List("data"), defaultSettings, data) must beRight(
        data.values
          .map {
            case ((labelName, labelValue), v) =>
              DimensionalMetricKey("data", List.empty, Map(labelName.toString -> labelValue.toString), Some(Counter)) -> Coproduct[
                Numbers
              ](v.value)
          }
      )
    }

  def testMultiDimensionalValues(
    implicit enc: TestMetricEncoder[Validation, CounterValues[Map[Short, Boolean], Int]]
  ): Prop =
    prop { data: CounterValues[Map[Short, Boolean], Int] =>
      enc.write(List("data"), defaultSettings, data) must beRight(data.values.map {
        case (labels, v) =>
          DimensionalMetricKey("data", List.empty, labels.map {
            case (labelName, labelValue) => labelName.toString -> labelValue.toString
          }, Some(Counter)) -> Coproduct[Numbers](v.value)
      })
    }

  def testObjectDimensionalValues(implicit enc: TestMetricEncoder[Validation, CounterValues[Dimensions, Int]]): Prop =
    prop { data: CounterValues[Dimensions, Int] =>
      enc.write(List("data"), defaultSettings, data) must beRight(data.values.map {
        case (dims, v) =>
          DimensionalMetricKey(
            "data",
            List.empty,
            Map("dimensionOne" -> dims.dimensionOne.toString, "dimensionTwo" -> dims.dimensionTwo.toString),
            Some(Counter)
          ) -> Coproduct[Numbers](v.value)
      })
    }

  override protected def mkEncoder[F[_], T](f: (List[String], Sett, T) => F[Metrics]): TestMetricEncoder[F, T] =
    new TestMetricEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Metrics] = f(path, settings, in)
    }
}

object MetricEncodersSpec {
  case class StatusCode(`200`: Int, `500`: Int, other: Int)
  case class RequestCount(httpRequests: StatusCode)

  case class HttpRequests(statusCode: CounterValues[String, Int])

  case class Both(a: RequestCount, b: HttpRequests)

  case class Dimensions(dimensionOne: Boolean, dimensionTwo: Short)

  trait TestMetricEncoder[F[_], T] extends MetricEncoder[F, MetricSettings, T]

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

  implicit val metricsEq: Eq[Metrics] = Eq.by(_.mapValues(Numbers.toLong))

  implicit def numArb(implicit ev: Arbitrary[Long]): Arbitrary[Numbers] =
    Arbitrary(ev.arbitrary.map(Coproduct[Numbers](_)))
}
