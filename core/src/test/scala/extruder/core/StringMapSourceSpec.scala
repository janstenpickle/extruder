package extruder.core

import cats.syntax.either._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.specification.core.SpecStructure

trait StringMapSourceSpec extends SourceSpec {
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

  override def ext2: SpecStructure =
    s2"""
        Can encode and decode a map $testMap
      """

  implicit def mapArb[A](implicit A: Arbitrary[A]): Arbitrary[Map[String, A]] =
    Arbitrary(
      Gen
        .listOfN(5, for {
          str <- Gen.alphaNumStr.map(_.toLowerCase).suchThat(_.nonEmpty)
          a <- A.arbitrary
        } yield (str, a))
        .map(_.toMap)
    )

  def testMap: Prop = prop { (map: Map[String, Int]) =>
    (for {
      enc <- encode[Map[String, Int]](List("a"), map)
      dec <- decode[Map[String, Int]](List("a"), enc)
    } yield dec) must beRight.which(_ === map)
  }

}
