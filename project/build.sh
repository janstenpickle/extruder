#!/bin/bash

sbtcmd="sbt ++${TRAVIS_SCALA_VERSION} clean dependencyUpdates scalastyle scalafmt::test compile "

for prj in "core" "refined" "typesafe"; do
  sbtcmd="${sbtcmd} \"project ${prj}\" coverage test coverageReport"
done

sbtcmd="${sbtcmd} \"project root\" coverageAggregate docs/makeMicrosite"

bash -c "${sbtcmd}"
