package extruder.metrics.prometheus

import extruder.metrics.data.CounterValue
import org.scalacheck.{Arbitrary, Gen}

object TestUtils {
  case class Metrics(a: CounterValue[Long], b: CounterValue[Long], c: CounterValue[Long])

  case class Stats(requests: Metrics)

  implicit val longArb: Arbitrary[Long] = Arbitrary(Gen.posNum[Long])
  implicit val strArb: Arbitrary[String] = Arbitrary(Gen.alphaStr.map(_.trim).suchThat(_.nonEmpty))
}
