package extruder.aws

import com.amazonaws.regions.{AwsRegionProvider, Region, Regions}
import extruder.core.{Parser, Show}

trait AwsRegionInstances {
  implicit val awsRegionParser: Parser[Region] =
    Parser.catchNonFatal(region => Region.getRegion(Regions.fromName(region)))

  implicit val awsRegionProviderParser: Parser[AwsRegionProvider] = awsRegionParser.map(
    region =>
      new AwsRegionProvider {
        override def getRegion: String = region.getName
    }
  )

  implicit val awsRegionShow: Show[Region] = Show.make(_.getName)

  implicit val awsRegionProviderShow: Show[AwsRegionProvider] = Show.make(_.getRegion)
}
