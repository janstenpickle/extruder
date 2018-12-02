package extruder.metrics

import java.util.concurrent.TimeUnit

import cats.{Eq, MonadError}
import cats.instances.long._
import cats.instances.map._
import cats.kernel.laws.discipline.MonoidTests
import extruder.data.Validation
import extruder.metrics.data.MetricType.Counter
import extruder.metrics.data._
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite}
import org.typelevel.discipline.scalatest.Discipline
import shapeless.Coproduct

import scala.concurrent.duration.FiniteDuration

class MetricEncodersSpec
    extends FunSuite
    with GeneratorDrivenPropertyChecks
    with EitherValues
    with Discipline
    with MetricEncoders {
  import MetricEncodersSpec._

  override type EncDefault[A] = Validation[A]
  override type EncT[F[_], T] = TestMetricEncoder[F, T]
  override type OutputData = Metrics
  override type EncEff[F[_]] = MonadError[F, Throwable]
  override type Sett = MetricSettings

  override def defaultSettings: MetricSettings = new MetricSettings {}

  private implicit val ev = monoid

  checkAll("Encoder monoid", MonoidTests[EncodeData].monoid)

  test("Can encode an Int")(testInt)
  test("Can encode an Long")(testLong)
  test("Can encode an Double")(testDouble)
  test("Can encode an Float")(testFloat)
  test("Can encode an FiniteDuration")(testFiniteDuration)
  test("Can encode a non-numeric value as a label")(testNonNum)
  test("Can encode a Map")(testMap)
  test("Can encode a numeric List")(testNumList)
  test("Can encode a non-numeric List")(testNonNumList)
  test("Can encode a List of Throwables")(testThrowableList)
  test("Can encode metric value")(testMetricValue)
  test("Can encode timer value")(testTimer)
  test("Can encode values")(testValues)
  test("Can encode metric values")(testValues)
  test("Can encode an object")(testObject)
  test("Can encode an object containing a values map")(testObjectMap)
  test("Can encode Both types of object in the same object")(testBoth)
  test("Can encode single dimensional map data")(testSingleDimensional)
  test("Can encode multi dimensional map data")(testMultiDimensional)
  test("Can encode object dimensional map data")(testObjectDimensional)
  test("Can encode single dimensional counter values")(testSingleDimensionalValues)
  test("Can encode multi dimensional counter values")(testMultiDimensionalValues)
  test("Can encode object dimensional counter values")(testObjectDimensionalValues)

  def testInt(implicit enc: TestMetricEncoder[Validation, Int]): Assertion = forAll { (i: Int, name: String) =>
    assert(
      enc.write(List(name), defaultSettings, i).right.value ===
        Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](i))
    )
  }

  def testLong(implicit enc: TestMetricEncoder[Validation, Long]): Assertion = forAll { (l: Long, name: String) =>
    assert(
      enc.write(List(name), defaultSettings, l).right.value ===
        Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](l))
    )
  }

  def testDouble(implicit enc: TestMetricEncoder[Validation, Double]): Assertion = forAll { (d: Double, name: String) =>
    assert(
      enc.write(List(name), defaultSettings, d).right.value ===
        Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](d))
    )
  }

  def testFloat(implicit enc: TestMetricEncoder[Validation, Float]): Assertion = forAll { (f: Float, name: String) =>
    assert(
      enc.write(List(name), defaultSettings, f).right.value ===
        Map(SimpleMetricKey(name, List.empty, None) -> Coproduct[Numbers](f))
    )
  }

  def testMap(implicit enc: TestMetricEncoder[Validation, Map[String, Int]]): Assertion = forAll {
    map: Map[String, Int] =>
      assert(enc.write(List.empty, defaultSettings, map).right.value === map.map {
        case (k, v) => SimpleMetricKey(k, List.empty, None) -> Coproduct[Numbers](v)
      })
  }

  def testFiniteDuration(implicit enc: TestMetricEncoder[Validation, FiniteDuration]): Assertion = forAll {
    (dur: FiniteDuration, name: String) =>
      assert(
        enc.write(List(name), defaultSettings, dur).right.value ===
          Map(SimpleMetricKey(name, List.empty, Some(MetricType.Timer)) -> Coproduct[Numbers](dur.toMillis))
      )
  }

  def testNonNum(implicit enc: TestMetricEncoder[Validation, String]): Assertion = forAll { (s: String, name: String) =>
    val short: Short = 1
    assert(
      enc.write(List(name), defaultSettings, s).right.value ===
        Map(SimpleMetricKey(s, List(name), Some(MetricType.Status)) -> Coproduct[Numbers](short))
    )
  }

  def testNumList(implicit enc: TestMetricEncoder[Validation, List[Int]]): Assertion = forAll {
    (li: List[Int], name: String) =>
      val key = SimpleMetricKey(name, List.empty, None)

      assert(
        enc.write(List(name), defaultSettings, li).right.value ===
          li.foldLeft(Map.empty[MetricKey, Numbers])(
            (acc, v) => acc + (key -> acc.get(key).fold(Coproduct[Numbers](v))(Numbers.add(_, Coproduct[Numbers](v))))
          )
      )
  }

  def testNonNumList(implicit enc: TestMetricEncoder[Validation, List[String]]): Assertion = forAll {
    (li: List[String], name: String) =>
      val short: Short = 1
      assert(enc.write(List(name), defaultSettings, li).right.value === li.foldLeft(Map.empty[MetricKey, Numbers]) {
        (acc, v) =>
          val key = SimpleMetricKey(v, List(name), Some(MetricType.Status))
          acc + (key -> acc.get(key).fold(Coproduct[Numbers](short))(Numbers.add(_, Coproduct[Numbers](short))))
      })
  }

  def testThrowableList(implicit enc: TestMetricEncoder[Validation, List[Throwable]]): Assertion = forAll {
    (li: List[Throwable], name: String) =>
      assert(
        enc.write(List(name), defaultSettings, li).right.value ===
          Map(SimpleMetricKey(name, List.empty, Some(MetricType.Gauge)) -> Coproduct[Numbers](li.size))
      )
  }

  def testMetricValue(implicit enc: TestMetricEncoder[Validation, MetricValue[Double]]): Assertion = forAll {
    (mv: MetricValue[Double], name: String) =>
      assert(
        enc.write(List(name), defaultSettings, mv).right.value ===
          Map(SimpleMetricKey(name, List.empty, Some(mv.metricType)) -> Coproduct[Numbers](mv.value))
      )
  }

  def testTimer(implicit enc: TestMetricEncoder[Validation, TimerValue[Long]]): Assertion = forAll {
    (start: Long, finish: Long, name: String) =>
      assert(
        enc.write(List(name), defaultSettings, TimerValue(start, Some(finish))).right.value ===
          Map(SimpleMetricKey(name, List.empty, Some(MetricType.Timer)) -> Coproduct[Numbers](finish - start))
      )
  }

  def testValues(implicit enc: TestMetricEncoder[Validation, MetricValues[MetricValue, String, Double]]): Assertion =
    forAll { values: Map[String, Double] =>
      assert(
        enc.write(List.empty, defaultSettings, MetricValues(values.mapValues(GaugeValue[Double]))).right.value ===
          values.map {
            case (k, v) => SimpleMetricKey(k, List.empty, Some(MetricType.Gauge)) -> Coproduct[Numbers](v)
          }
      )
    }

  def testObject(implicit enc: TestMetricEncoder[Validation, RequestCount]): Assertion = forAll { rq: RequestCount =>
    assert(
      enc.write(List.empty, defaultSettings, rq).right.value ===
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

  def testObjectMap(implicit enc: TestMetricEncoder[Validation, HttpRequests]): Assertion = forAll { rq: HttpRequests =>
    assert(enc.write(List.empty, defaultSettings, rq).right.value === rq.statusCode.values.map {
      case (k, v) =>
        DimensionalMetricKey("HttpRequests", List.empty, Map("statusCode" -> k), Some(MetricType.Counter)) -> Coproduct[
          Numbers
        ](v.value)
    })
  }

  def testBoth(implicit enc: TestMetricEncoder[Validation, Both]): Assertion = forAll { rq: Both =>
    assert(
      enc.write(List.empty, defaultSettings, rq).right.value ===
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

  def testSingleDimensional(implicit enc: TestMetricEncoder[Validation, Map[(Short, Int), Int]]): Assertion = forAll {
    data: Map[(Short, Int), Int] =>
      assert(enc.write(List("data"), defaultSettings, data).right.value === data.map {
        case ((labelName, labelValue), v) =>
          DimensionalMetricKey("data", List.empty, Map(labelName.toString -> labelValue.toString), None) -> Coproduct[
            Numbers
          ](v)
      })
  }

  def testMultiDimensional(implicit enc: TestMetricEncoder[Validation, Map[Map[Short, Boolean], Int]]): Assertion =
    forAll { data: Map[Map[Short, Boolean], Int] =>
      assert(enc.write(List("data"), defaultSettings, data).right.value === data.map {
        case (labels, v) =>
          DimensionalMetricKey("data", List.empty, labels.map {
            case (labelName, labelValue) => labelName.toString -> labelValue.toString
          }, None) -> Coproduct[Numbers](v)
      })
    }

  def testObjectDimensional(implicit enc: TestMetricEncoder[Validation, Map[Dimensions, Int]]): Assertion = forAll {
    data: Map[Dimensions, Int] =>
      assert(enc.write(List("data"), defaultSettings, data).right.value === data.map {
        case (dims, v) =>
          DimensionalMetricKey(
            "data",
            List.empty,
            Map("dimensionOne" -> dims.dimensionOne.toString, "dimensionTwo" -> dims.dimensionTwo.toString),
            None
          ) -> Coproduct[Numbers](v)
      })
  }

  def testSingleDimensionalValues(
    implicit enc: TestMetricEncoder[Validation, CounterValues[(Short, Int), Int]]
  ): Assertion =
    forAll { data: CounterValues[(Short, Int), Int] =>
      assert(
        enc.write(List("data"), defaultSettings, data).right.value ===
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
  ): Assertion =
    forAll { data: CounterValues[Map[Short, Boolean], Int] =>
      assert(enc.write(List("data"), defaultSettings, data).right.value === data.values.map {
        case (labels, v) =>
          DimensionalMetricKey("data", List.empty, labels.map {
            case (labelName, labelValue) => labelName.toString -> labelValue.toString
          }, Some(Counter)) -> Coproduct[Numbers](v.value)
      })
    }

  def testObjectDimensionalValues(
    implicit enc: TestMetricEncoder[Validation, CounterValues[Dimensions, Int]]
  ): Assertion =
    forAll { data: CounterValues[Dimensions, Int] =>
      assert(enc.write(List("data"), defaultSettings, data).right.value === data.values.map {
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
