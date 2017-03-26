package extruder.core

import java.net.URL

import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration.{Duration, FiniteDuration}

object TestCommon {
  val nonEmptyStringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  val namespaceGen: Gen[List[String]] = Gen.listOf(nonEmptyStringGen).suchThat(_.nonEmpty)

  val urlGen: Gen[URL] = for {
    host <- nonEmptyStringGen
    path <- nonEmptyStringGen
  } yield new URL(s"http://$host/$path")

  implicit val urlArb: Arbitrary[URL] = Arbitrary(urlGen)

  val finiteDurationGen: Gen[FiniteDuration] = Gen.chooseNum(0L, 5000L).map(Duration.fromNanos)

  val durationGen: Gen[Duration] = Gen.oneOf(
    finiteDurationGen,
    Gen.const(Duration.Inf),
    Gen.const(Duration.MinusInf),
    Gen.const(Duration.Zero)
  )
}
