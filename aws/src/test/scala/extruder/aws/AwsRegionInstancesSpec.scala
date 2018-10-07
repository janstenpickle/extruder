package extruder.aws

import com.amazonaws.regions.{AwsRegionProvider, Region, Regions}
import extruder.aws.region._
import extruder.core.{Parser, Show}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite}

class AwsRegionInstancesSpec extends FunSuite with GeneratorDrivenPropertyChecks with EitherValues {
  import AwsRegionInstancesSpec._

  test("Parses a valid region")(passes)
  test("Fails to parse an invalid region")(fails)
  test("Shows a region")(shows)

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

object AwsRegionInstancesSpec {
  implicit val regionArb: Arbitrary[Region] = Arbitrary(Gen.oneOf(Regions.values().toList.map(Region.getRegion)))
}
