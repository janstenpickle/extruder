#!/bin/bash

sbtcmd="sbt clean "

for prj in "core" "catsEffect" "aws" "refined" "typesafe" "prometheus" "spectator" "dropwizard" "metricsCore"; do
  sbtcmd="${sbtcmd} \"project ${prj}\" coverage test coverageReport"
done

bash -c "${sbtcmd}"
