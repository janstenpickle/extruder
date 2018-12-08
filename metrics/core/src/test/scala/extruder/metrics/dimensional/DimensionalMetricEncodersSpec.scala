package extruder.metrics.dimensional

import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.functor._
import cats.instances.string._
import cats.{Applicative, Eq, MonadError}
import extruder.core.{DataSource, Encode, ExtruderErrors}
import extruder.data.{Finalize, Validation, ValidationError, ValidationErrors}
import extruder.metrics.MetricEncodersSpec.{Dimensions, RequestCount, StatusCode}
import extruder.metrics._
import extruder.metrics.data._
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite}
import shapeless.Coproduct
import utest.compileError

class DimensionalMetricEncodersSpec extends FunSuite with GeneratorDrivenPropertyChecks with EitherValues {
  import DimensionalMetricEncodersSpec._
  import extruder.metrics.MetricEncodersSpec._
  import TestDimensionalEncoders._

  test("Can encode an object")(testObject)
  test("Can encode a map where each key becomes a metric name and the namespace is empty")(noLabel)
  test(
    "Can encode a map within a case class where the name of the case class is the metric name and the map keys become label values"
  )(withLabel)
  test("Can encode statuses where the status value appears as the metric name")(status)
  test("Can encode multidimensional data")(testMultiDimensional)
  test("Can reset the namespace at a lower level in the case class tree")(testResetNamespace)
  test("Can encode a status within a case class")(testStatus)
  test("Can encode error counts")(testErrors)
  test("Raises an error when two metrics under the same name are created with different types")(testFailure)
  test("Fails to compile an object with different numeric types")(testDifferentNumericTypeFail)
  test("Fails to compile an object with different metric value types")(testDifferentValueTypeFail)

  val counterSettings = new DimensionalMetricSettings {
    override def defaultMetricType: MetricType = MetricType.Counter
  }

  def testObject: Assertion = forAll { (c: Both) =>
    val metrics = encode(counterSettings, c).right.value
    assert((metrics.size === 1) && metrics.head.values.size === c.b.statusCode.values.size + 3)
  }

  def noLabel: Assertion = forAll { (m: Map[String, Int]) =>
    val metrics = encode(m).right.value
    assert((metrics.size === m.size) && (metrics.forall(_.values.head._1.size === 1)))
  }

  def withLabel: Assertion = forAll { (es: EncoderStats) =>
    val metrics = encode(es).right.value
    assert((metrics.size === es.`type`.lastOption.fold(0)(_ => 1)) && metrics.headOption.fold[Boolean](1 === 1) {
      metric =>
        metric.name === "encoder_stats"
    })
  }

  def status: Assertion = forAll { (a: All) =>
    val metrics = encode(a).right.value
    val short: Short = 1
    val status = metrics.filter(_.metricType == MetricType.Status).head
    assert(
      (metrics.size == 2) && (status.name === defaultSettings
        .labelTransform(a.someStatus)) && (status.values.size === 1) && (status.values.head._2 === Coproduct[Numbers](
        short
      ))
    )
  }

  def testMultiDimensional: Assertion = forAll { data: DimensionalData =>
    val metrics = encode(List("ns"), data).right.value
    assert(
      (metrics.size === data.data.headOption.fold(0)(_ => 1)) &&
        (
          metrics.map(_.name).toSet.headOption === Some("data")
            .filter(_ => data.data.nonEmpty)
        ) && (metrics.flatMap(_.values.values.flatMap(_.select[Int])).sum === data.data.values.sum)
    )
  }

  def testResetNamespace: Assertion = forAll { (rq: Reset) =>
    val metrics = encode(rq).right.value
    assert((metrics.size === 2) && (metrics.map(_.name) === List("b", "a")))
  }

  def testStatus: Assertion = forAll { (stats: EncoderStats2) =>
    val metrics = encode(stats).right.value
    val short: Short = 1
    (metrics.map(_.name) should contain)
      .theSameElementsAs(List("http_requests", defaultSettings.labelTransform(stats.jobStatus)))
    assert(
      (metrics.size === 2)
        && (metrics.filter(_.metricType === MetricType.Status).head.values.head._2 === Coproduct[Numbers](short))
    )
  }

  def testErrors: Assertion = forAll { (stats: EncoderStats3) =>
    val metrics = encode(stats).right.value

    val errors = metrics.collectFirst { case m: DimensionalMetric if m.name == "errors" => m.values }.get
    assert((metrics.size === 2) && (errors.size === 1) && (errors.head._2 === Coproduct[Numbers](stats.errors.size)))
  }

  def testFailure: Assertion = forAll { (fail: Fail) =>
    assert(
      encode(fail).left.value ===
        NonEmptyList.of(
          ValidationError.failure(
            "Multiple metric types for the following metrics, please ensure there is just one: 'a' -> 'Timer, Counter'"
          )
        )
    )
  }

  def testDifferentNumericTypeFail: Assertion = forAll { (dt: DifferentTypes) =>
    assert(
      Either
        .catchNonFatal(
          compileError("encodeF[extruder.data.Validation](dt)").check(
            "",
            "could not find implicit value for parameter encoder: extruder.core.EncoderT" +
              "[extruder.data.Validation,extruder.metrics.dimensional.DimensionalMetricEncodersSpec.TestDimensionalEncoders.Sett," +
              "extruder.metrics.dimensional.DimensionalMetricEncodersSpec.DifferentTypes," +
              "extruder.metrics.dimensional.DimensionalMetricEncodersSpec.TestDimensionalEncoders.EncodeData]"
          )
        )
        .isRight
    )
  }

  def testDifferentValueTypeFail: Assertion = forAll { (dt: DifferentTypes2) =>
    assert(
      Either
        .catchNonFatal(
          compileError("encodeF[extruder.data.Validation](dt)").check(
            "",
            "could not find implicit value for parameter encoder: extruder.core.EncoderT" +
              "[extruder.data.Validation,extruder.metrics.dimensional.DimensionalMetricEncodersSpec.TestDimensionalEncoders.Sett," +
              "extruder.metrics.dimensional.DimensionalMetricEncodersSpec.DifferentTypes2," +
              "extruder.metrics.dimensional.DimensionalMetricEncodersSpec.TestDimensionalEncoders.EncodeData]"
          )
        )
        .isRight
    )
  }

}

object DimensionalMetricEncodersSpec {
  case class All(reqs: RequestCount, someStatus: String)

  case class EncoderStats(`type`: Map[String, Int])

  case class Nested(a: ResetNamespace[Int], b: ResetNamespace[Int])

  case class Reset(a: Nested)

  case class EncoderStats2(httpRequests: StatusCode, jobStatus: String)

  case class EncoderStats3(httpRequests: StatusCode, errors: List[Throwable])

  case class DifferentTypes(i: Int, l: Long)

  case class DifferentTypes2(gauge: GaugeValue[Long], counter: CounterValue[Long])

  case class Counter(a: ResetNamespace[CounterValue[Int]])
  case class Timer(a: ResetNamespace[TimerValue[Long]])

  case class Fail(one: Counter, two: Timer)

  case class DimensionalData(data: Map[Dimensions, Int])

//  trait TestDimensionalMetricEncoder[F[_], T] extends MetricEncoder[F, DimensionalMetricSettings, T]

  implicit val validationErrorEq: Eq[ValidationError] = Eq.by(_.message)

  object TestDimensionalEncoders extends Encode with DimensionalMetricEncoderInstances with DataSource {
    override type EncodeData = Metrics
    override type OutputData = Iterable[DimensionalMetric]
    override type EncodeDefault[A] = Validation[A]
    override type Sett = DimensionalMetricSettings
    override def defaultSettings: Sett =
      new DimensionalMetricSettings {}
  }
}
