package extruder.refined

import cats.MonadError
import cats.syntax.either._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.scalacheck.boolean._
import eu.timepit.refined.scalacheck.generic._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.scalacheck.char._
import eu.timepit.refined.scalacheck.any._
import extruder.core._
import extruder.data.Validation
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Assertion, EitherValues, FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class RefinedInstancesSpec
    extends FunSuite
    with GeneratorDrivenPropertyChecks
    with EitherValues
    with Matchers
    with MapEncoders
    with MapDecoders {

  override type DecEff[F[_]] = MonadError[F, Throwable]
  override type EncEff[F[_]] = MonadError[F, Throwable]

  type ZeroToOne = Not[Less[W.`0.0`.T]] And Not[Greater[W.`1.0`.T]]
  type RegexMatch = MatchesRegex[W.`"[^0-9]+"`.T]

  test("Can encode and decode Int Refined Positive") { encodeDecode[Int Refined Positive] }
  test("Can encode and decode Int Refined Greater[W.`5`.T]") { encodeDecode[Int Refined Greater[W.`5`.T]] }
  test("Can encode and decode Double Refined ZeroToOne") { encodeDecode[Double Refined ZeroToOne] }
  test("Can encode and decode String Refined NonEmpty") { encodeDecode[String Refined NonEmpty] }
  test("Can encode and decode String Refined RegexMatch") { encodeDecode[String Refined RegexMatch] }
  test("Can encode and decode Char Refined Letter") { encodeDecode[Char Refined Letter] }
  test("Can encode and decode Char Refined Digit") { encodeDecode[Char Refined Digit] }
  test("Can encode and decode List Refined NonEmpty") { encodeDecode[List[Int] Refined NonEmpty] }

  test("Fails to decode Int Refined Positive") { failEncodeDecode[Int Refined Positive, Int](Gen.negNum[Int]) }
  test("Fails to decode Int Refined Greater[W.`5`.T]") {
    failEncodeDecode[Int Refined Greater[W.`5`.T], Int](Gen.negNum[Int])
  }
  test("Fails to decode Double Refined ZeroToOne") {
    failEncodeDecode[Double Refined ZeroToOne, Double](Gen.negNum[Double])
  }
  test("Fails to decode String Refined NonEmpty") { failEncodeDecode[String Refined NonEmpty, String](Gen.const("")) }
  test("Fails to decode Char Refined Letter") { failEncodeDecode[Char Refined Letter, Short](Gen.choose(0, 9)) }
  test("Fails to decode Char Refined Digit") { failEncodeDecode[Char Refined Digit, Char](Gen.alphaChar) }
  test("Fails to decode List Refined NonEmpty") { failEncodeDecode[List[Int] Refined NonEmpty, String](Gen.const("")) }

  def encodeDecode[T](
    implicit arb: Arbitrary[T],
    encoder: MapEncoder[Validation, T],
    decoder: MapDecoder[Validation, T]
  ): Assertion =
    forAll(
      (v: T) =>
        assert((for {
          encoded <- encodeF[Validation](v)
          decoded <- decodeF[Validation, T](encoded)
        } yield decoded).right.value === v)
    )

  def failEncodeDecode[T, V](gen: Gen[V])(implicit decoder: MapDecoder[Validation, T]): Assertion =
    forAll(gen) { src =>
      val failure = decodeF[Validation, T](Map("" -> src.toString)).left.value
      (failure.toList should have).size(1)
      failure.toList.head.message.toLowerCase should include("predicate")
    }
}
