package extruder.core

import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion

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
      enc <- encode[Map[String, Int]](List("a"), map)
      dec <- decode[Map[String, Int]](List("a"), enc)
    } yield dec).map(_ === map).right.value)
  }

}
