package extruder.refined

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
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class RefinedInstancesSpec
    extends Specification
    with ScalaCheck
    with EitherMatchers
    with MapEncoders
    with MapDecoders {
  type ZeroToOne = Not[Less[W.`0.0`.T]] And Not[Greater[W.`1.0`.T]]
  type RegexMatch = MatchesRegex[W.`"[^0-9]+"`.T]

  override def is: SpecStructure =
    s2"""
       Can encode and decode refined types
        Int Refined Positive ${encodeDecode[Int Refined Positive]}
        Int Refined Greater[W.`5`.T] ${encodeDecode[Int Refined Greater[W.`5`.T]]}
        Double Refined ZeroToOne ${encodeDecode[Double Refined ZeroToOne]}
        String Refined NonEmpty ${encodeDecode[String Refined NonEmpty]}
        String Refined RegexMatch ${encodeDecode[String Refined RegexMatch]}
        Char Refined Letter ${encodeDecode[Char Refined Letter]}
        Char Refined Digit ${encodeDecode[Char Refined Digit]}
        List Refined NonEmpty ${encodeDecode[List[Int] Refined NonEmpty]}

       Fails to decode a value which does not conform to refined predicate
        Int Refined Positive ${failEncodeDecode[Int Refined Positive, Int](Gen.negNum[Int])}
        Int Refined Greater[W.`5`.T] ${failEncodeDecode[Int Refined Greater[W.`5`.T], Int](Gen.negNum[Int])}
        Double Refined ZeroToOne ${failEncodeDecode[Double Refined ZeroToOne, Double](Gen.negNum[Double])}
        String Refined NonEmpty ${failEncodeDecode[String Refined NonEmpty, String](Gen.const(""))}
        Char Refined Letter ${failEncodeDecode[Char Refined Letter, Short](Gen.choose(0, 9))}
        Char Refined Digit ${failEncodeDecode[Char Refined Digit, Char](Gen.alphaChar)}
        List Refined NonEmpty ${failEncodeDecode[List[Int] Refined NonEmpty, String](Gen.const(""))}
      """

  def encodeDecode[T](
    implicit arb: Arbitrary[T],
    encoder: MapEncoder[Validation, T],
    decoder: MapDecoder[Validation, T]
  ): Prop =
    prop(
      (v: T) =>
        (for {
          encoded <- encode[T](v)
          decoded <- decode[T](encoded)
        } yield decoded) must beRight(v)
    )

  def failEncodeDecode[T, V](gen: Gen[V])(implicit decoder: MapDecoder[Validation, T]): Prop =
    Prop.forAll(gen) { src =>
      decode[T](Map("" -> src.toString)) must beLeft.which(
        failure =>
          (failure.toList must haveSize(1)).and(failure.toList.head.message.toLowerCase must contain("predicate"))
      )
    }
}
