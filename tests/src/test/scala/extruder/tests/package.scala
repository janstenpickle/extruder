package extruder

import org.scalacheck.{Arbitrary, Gen}

package object tests {
  // implicit val nonEmptyStringArb: Arbitrary[String] = Arbitrary(Gen.alphaNumStr.suchThat(_.nonEmpty))

//  implicit val namespaceArb: Arbitrary[List[String]] = Arbitrary(Gen.nonEmptyListOf(nonEmptyStringArb.arbitrary))
}
