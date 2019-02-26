#!/bin/bash

export GEM_HOME=${HOME}/.gem
export PATH=${PATH}:${GEM_HOME}/bin
sudo apt-get update
sudo apt-get install -y --no-install-recommends ruby ruby-dev
mkdir -p ${GEM_HOME}/gems
gem install sass
gem install -f jekyll -v 3.8.5

sbt -mem 3000 ++${SCALA_VERSION} docs/makeMicrosite
