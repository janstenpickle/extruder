package extruder.core

import java.net.URL

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.all.{catsStdEqForTry, _}
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

object TestCommon {
  val nonEmptyStringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  val namespaceGen: Gen[List[String]] = Gen.nonEmptyListOf(nonEmptyStringGen)

  val urlGen: Gen[URL] = for {
    host <- nonEmptyStringGen
    path <- nonEmptyStringGen
  } yield new URL(s"http://$host/$path")

  implicit val urlArb: Arbitrary[URL] = Arbitrary(urlGen)

  val finiteDurationGen: Gen[FiniteDuration] = Gen.chooseNum(0L, 5000L).map(Duration.fromNanos)

  val durationGen: Gen[Duration] =
    Gen.oneOf(finiteDurationGen, Gen.const(Duration.Inf), Gen.const(Duration.MinusInf), Gen.const(Duration.Zero))

  def nonEmptyListGen[T](gen: Gen[T]): Gen[NonEmptyList[T]] =
    for {
      head <- gen
      tail <- Gen.listOf(gen)
    } yield NonEmptyList.of(head, tail: _*)

  implicit def tryEq[A](implicit e: Eq[A]): Eq[Try[A]] =
    catsStdEqForTry[A, Throwable](e, Eq.by[Throwable, String](_.toString))
}
