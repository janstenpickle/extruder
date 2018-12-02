package extruder.core

import cats.kernel.laws.discipline.MonoidTests
import extruder.data.Validation
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion
import cats.syntax.flatMap._
import cats.syntax.functor._

abstract class StringMapSourceSuite[A](monoidTests: MonoidTests[A]#RuleSet) extends SourceSuite(monoidTests) {
  self: Encode
    with Encoders
    with PrimitiveEncoders
    with StringMapEncoders
    with DerivedEncoders
    with EncodeTypes
    with Decode
    with Decoders
    with DecodeFromDefaultSource
    with PrimitiveDecoders
    with StringMapDecoders
    with DerivedDecoders
    with DecodeTypes =>

  test("Can encode and decode a map") { testMap }

  implicit def mapArb[A](implicit A: Arbitrary[A]): Arbitrary[Map[String, A]] =
    Arbitrary(
      Gen
        .listOfN(5, for {
          str <- Gen.alphaNumStr.map(_.toLowerCase).suchThat(_.nonEmpty)
          a <- A.arbitrary
        } yield (str, a))
        .map(_.toMap)
    )

  def testMap: Assertion = forAll { map: Map[String, Int] =>
    assert((for {
      enc <- encodeF[Validation](List("a"), map)
      dec <- decodeF[Validation, Map[String, Int]](List("a"), enc)
    } yield dec).map(_ === map).right.value)
  }

}
