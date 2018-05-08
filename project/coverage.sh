#!/bin/bash

sbtcmd="sbt clean "

for prj in "core" "aws" "refined" "typesafe" "prometheus" "spectator" "dropwizard" "metricsCore"; do
  sbtcmd="${sbtcmd} \"project ${prj}\" coverage test coverageReport"
done

sbtcmd="${sbtcmd} \"project root\" coverageAggregate"

bash -c "${sbtcmd}"
