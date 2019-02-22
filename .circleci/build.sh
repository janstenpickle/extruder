#!/bin/bash

sbtcmd="sbt -mem 2500 ++${SCALA_VERSION} clean dependencyUpdates scalafmt::test compile "

for prj in "core" "catsEffect" "circe" "circeYaml" "tests" "aws" "refined" "typesafe" "prometheus" "spectator" "dropwizard" "metricsCore"; do
  sbtcmd="${sbtcmd} \"project ${prj}\" coverage test coverageReport"
done

n=0
until [ $n -ge 3 ]
do
  bash -c "${sbtcmd}" && break
  n=$[$n+1]
  sleep 15
done
