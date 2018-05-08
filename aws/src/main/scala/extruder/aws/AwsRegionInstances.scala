package extruder.aws

import com.amazonaws.regions.{Region, Regions}
import extruder.core.{Parser, Show}

trait AwsRegionInstances {
  implicit def awsRegionParser: Parser[Region] =
    Parser.catchNonFatal(region => Region.getRegion(Regions.fromName(region)))

  implicit def awsRegionShow: Show[Region] = Show { r: Region =>
    r.getName
  }
}
