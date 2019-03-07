#!/bin/bash

sbtcmd="sbt -mem 2500 ++${SCALA_VERSION} clean dependencyUpdates scalafmt::test compile "

for prj in "core" "catsEffect" "circe" "circeYaml" "tests" "aws" "refined" "typesafe" "prometheus" "spectator" "dropwizard" "metricsCore"; do
  sbtcmd="${sbtcmd} \"project ${prj}\" coverage test coverageReport"
done


function runSbt() {
  bash -c "${sbtcmd}"
  return $?
}

retry=0
maxRetries=5
until [ ${retry} -ge ${maxRetries} ]
do
	runSbt && break
	retry=$[${retry}+1]
	echo "Retrying [${retry}/${maxRetries}] in ${retryInterval}(s) "
done

if [ ${retry} -ge ${maxRetries} ]; then
  echo "Failed after ${maxRetries} attempts!"
  exit 1
fi

