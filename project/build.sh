#!/bin/bash

sbtcmd="sbt ++${TRAVIS_SCALA_VERSION} clean dependencyUpdates scalafmt::test compile "

for prj in "core" "catsEffect" "tests" "aws" "refined" "typesafe" "prometheus" "spectator" "dropwizard" "metricsCore"; do
  sbtcmd="${sbtcmd} \"project ${prj}\" coverage test coverageReport"
done

if [[ $TRAVIS_SCALA_VERSION = "2.12"* ]]; then
  sbtcmd="${sbtcmd} docs/makeMicrosite"
fi

bash -c "${sbtcmd}"
