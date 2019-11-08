package extruder.aws

import cats.Eq
import cats.instances.string._
import com.amazonaws.regions.{AwsRegionProvider, Region, Regions}
import extruder.aws.region._
import extruder.core.{Parser, Show}
import extruder.laws.ParserShowTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Assertion, EitherValues}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline

class AwsRegionInstancesSuite
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with Discipline {
  import AwsRegionInstancesSuite._

  test("Parses a valid region")(passes)
  test("Fails to parse an invalid region")(fails)
  test("Shows a region")(shows)

  checkAll("Region", ParserShowTests[Region].parserShow)
  checkAll("AwsRegionProvider", ParserShowTests[AwsRegionProvider].parserShow)

  def passes = {
    passesTest[Region]
    passesTest[AwsRegionProvider]
  }

  def passesTest[A: Parser]: Assertion = forAll { region: Region =>
    assert(Parser[Region].parse(region.getName).right.value === region)
  }

  def fails = {
    failsTest[Region]
    passesTest[AwsRegionProvider]
  }

  def failsTest[A: Parser]: Assertion = forAll { str: String =>
    assert(Parser[Region].parse(str).left.value === s"Cannot create enum from $str value!")
  }

  def shows = {
    showsTest[Region]
    showsTest[AwsRegionProvider]
  }

  def showsTest[A: Show]: Assertion = forAll { region: Region =>
    assert(Show[Region].show(region) === region.getName)
  }
}

object AwsRegionInstancesSuite {
  implicit val regionArb: Arbitrary[Region] = Arbitrary(Gen.oneOf(Regions.values().toList.map(Region.getRegion)))
  implicit val regionProviderArb: Arbitrary[AwsRegionProvider] = Arbitrary(
    regionArb.arbitrary.map(
      region =>
        new AwsRegionProvider {
          override def getRegion: String = region.getName
      }
    )
  )

  implicit val regionEq: Eq[Region] = Eq.by(_.getName)
  implicit val regionProviderEq: Eq[AwsRegionProvider] = Eq.by(_.getRegion)
}
