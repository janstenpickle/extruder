package extruder.aws

import com.amazonaws.regions.{Region, Regions}
import org.specs2.{ScalaCheck, Specification}
import extruder.aws.region._
import extruder.core.{Parser, Show}
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.specification.core.SpecStructure

class AwsRegionInstancesSpec extends Specification with ScalaCheck {
  import AwsRegionInstancesSpec._

  override def is: SpecStructure =
    s2"""
        Parses a valid region $passes
        Fails to parse an invalid region $fails

        Shows a region $shows
      """

  def passes: Prop = prop { region: Region =>
    Parser[Region].parse(region.getName) must beRight(region)
  }

  def fails: Prop = prop { str: String =>
    Parser[Region].parse(str) must beLeft(s"Cannot create enum from $str value!")
  }

  def shows: Prop = prop { region: Region =>
    Show[Region].show(region) === region.getName
  }
}

object AwsRegionInstancesSpec {
  implicit val regionArb: Arbitrary[Region] = Arbitrary(Gen.oneOf(Regions.values().toList.map(Region.getRegion)))
}
