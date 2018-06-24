package extruder.metrics.dimensional

import cats.data.NonEmptyList
import cats.syntax.either._
import extruder.core.{Encode, ValidationFailure}
import extruder.effect.ExtruderMonadError
import extruder.metrics.MetricEncodersSpec.{Dimensions, RequestCount, StatusCode}
import extruder.metrics._
import extruder.metrics.data._
import org.scalacheck.Prop
import org.scalacheck.ScalacheckShapeless._
import org.specs2.matcher.{EitherMatchers, MatchResult, Matchers}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import shapeless.Coproduct
import utest.compileError

class DimensionalMetricEncodersSpec
    extends Specification
    with ScalaCheck
    with Matchers
    with EitherMatchers
    with DimensionalMetricEncoders
    with Encode {
  import DimensionalMetricEncodersSpec._
  import extruder.metrics.MetricEncodersSpec._

  override type Enc[F[_], T] = TestDimensionalMetricEncoder[F, T]
  override type OutputData = Iterable[DimensionalMetric]
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Sett = DimensionalMetricSettings

  override def defaultSettings: DimensionalMetricSettings = new DimensionalMetricSettings {}

  override protected def mkEncoder[F[_], T](
    f: (List[String], Sett, T) => F[Metrics]
  ): TestDimensionalMetricEncoder[F, T] =
    new TestDimensionalMetricEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Metrics] = f(path, settings, in)
    }

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: Metrics)(
    implicit F: ExtruderMonadError[F]
  ): F[Iterable[DimensionalMetric]] =
    buildMetrics(Some("namespace"), namespace, settings, inter, Map.empty, MetricType.Counter)

  override def is: SpecStructure =
    s2"""
        Can encode an object $testObject
        Can encode a map where each key becomes a metric name and the namespace is empty $noLabel
        Can encode a map within a case class where the name of the case class is the metric name and the map keys become label values $withLabel
        Can encode statuses where the status value appears as the metric name $status
        Can encode multidimensional data $testMultiDimensional
        Can reset the namespace at a lower level in the case class tree $testResetNamespace
        Can encode a status within a case class $testStatus
        Can encode error counts $testErrors
        Raises an error when two metrics under the same name are created with different types $testFailure
        Fails to compile an object with different numeric types $testDifferentNumericTypeFail
        Fails to compile an object with different metric value types $testDifferentValueTypeFail
      """

  def testObject: Prop = prop { (c: Both) =>
    encode[Both](c) must beRight[Iterable[DimensionalMetric]]
      .which { metrics =>
        (metrics.size === 1).and(metrics.head.values.size === c.b.statusCode.values.size + 3)
      }
  }

  def noLabel: Prop = prop { (m: Map[String, Int]) =>
    encode[Map[String, Int]](m) must beRight.which { metrics =>
      (metrics.size === m.size).and(metrics.forall(_.values.head._1.head.isEmpty))
    }
  }

  def withLabel: Prop = prop { (es: EncoderStats) =>
    encode[EncoderStats](es) must beRight.which { metrics =>
      (metrics.size === es.`type`.lastOption.fold(0)(_ => 1))
        .and(metrics.headOption.fold[MatchResult[_]](1 === 1) { metric =>
          metric.name === "encoder_stats"
        })
    }
  }

  def status: Prop = prop { (a: All) =>
    encode[All](a) must beRight.which { metrics =>
      val short: Short = 1
      val status = metrics.filter(_.metricType == MetricType.Status).head
      (metrics.size == 2)
        .and(status.name === defaultSettings.labelTransform(a.someStatus))
        .and(status.values.size === 1)
        .and(status.values.head._2 === Coproduct[Numbers](short))

    }
  }

  def testMultiDimensional: Prop = prop { data: DimensionalData =>
    encode[DimensionalData](List("ns"), data) must beRight
      .which { metrics =>
        (metrics.size === data.data.headOption.fold(0)(_ => 1))
          .and(
            metrics.map(_.name).toSet.headOption === Some("data")
              .filter(_ => data.data.nonEmpty)
          )
          .and(metrics.flatMap(_.values.values.flatMap(_.select[Int])).sum === data.data.values.sum)
      }
  }

  def testResetNamespace: Prop = prop { (rq: Reset) =>
    encode[Reset](rq) must beRight.which { metrics =>
      (metrics.size === 2).and(metrics.map(_.name) === List("b", "a"))
    }
  }

  def testStatus: Prop = prop { (stats: EncoderStats2) =>
    encode[EncoderStats2](stats) must beRight.which { metrics =>
      val short: Short = 1
      (metrics.size === 2)
        .and(
          metrics.map(_.name) must containTheSameElementsAs(
            List(defaultSettings.labelTransform(stats.jobStatus), "http_requests")
          )
        )
        .and(metrics.filter(_.metricType === MetricType.Status).head.values.head._2 === Coproduct[Numbers](short))
    }
  }

  def testErrors: Prop = prop { (stats: EncoderStats3) =>
    encode[EncoderStats3](stats) must beRight.which { metrics =>
      val errors = metrics.collectFirst { case m: DimensionalMetric if m.name == "errors" => m.values }.get
      (metrics.size === 2)
        .and(errors.size === 1)
        .and(errors.head._2 === Coproduct[Numbers](stats.errors.size))
    }
  }

  def testFailure: Prop = prop { (fail: Fail) =>
    encode[Fail](fail) must beLeft(
      NonEmptyList.of(
        ValidationFailure(
          "Multiple metric types for the following metrics, please ensure there is just one: 'a' -> 'Timer, Counter'"
        )
      )
    )
  }

  def testDifferentNumericTypeFail: Prop = prop { (dt: DifferentTypes) =>
    Either.catchNonFatal(
      compileError("encode[extruder.core.Validation, DifferentTypes](dt)").check(
        "",
        "could not find implicit value for parameter encoder: DimensionalMetricEncodersSpec.this.Enc" +
          "[extruder.core.Validation,extruder.metrics.dimensional.DimensionalMetricEncodersSpec.DifferentTypes]"
      )
    ) must beRight
  }

  def testDifferentValueTypeFail: Prop = prop { (dt: DifferentTypes2) =>
    Either.catchNonFatal(
      compileError("encode[extruder.core.Validation, DifferentTypes2](dt)").check(
        "",
        "could not find implicit value for parameter encoder: DimensionalMetricEncodersSpec.this.Enc" +
          "[extruder.core.Validation,extruder.metrics.dimensional.DimensionalMetricEncodersSpec.DifferentTypes2]"
      )
    ) must beRight
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

  trait TestDimensionalMetricEncoder[F[_], T] extends MetricEncoder[F, DimensionalMetricSettings, T]

}
