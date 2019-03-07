<div style="text-align:center"><img src ="https://janstenpickle.github.io/extruder/img/extruder.svg" /></div>

# Extruder

[![Typelevel Incubator](https://img.shields.io/badge/typelevel-incubator-green.svg)](http://typelevel.org/projects) [![CircleCI](https://circleci.com/gh/janstenpickle/extruder/tree/master.svg?style=svg)](https://circleci.com/gh/janstenpickle/extruder/tree/master) [![codecov](https://codecov.io/gh/janstenpickle/extruder/branch/master/graph/badge.svg)](https://codecov.io/gh/janstenpickle/extruder)

This library uses [shapeless](https://github.com/milessabin/shapeless) and [cats](https://github.com/typelevel/cats) to provide a neat syntax to instantiate Scala case classes from a data source.

[See the extruder microsite for detailed documentation.](https://janstenpickle.github.io/extruder/)

# Modules
|Module|Description|Download|
|---|---|---|
| **Extruder**|Main module, includes core functionality and basic resolvers.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/io.extruder/_latestVersion)|
| **Cats Effect**|Provides Cats Effect [typeclass implementations](concepts.html#effects).|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/io.extruder/_latestVersion)|
| **Typesafe Config**|Support for resolution from [Typesafe Config](https://github.com/typesafehub/config).|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-typesafe/_latestVersion)|
| **Circe**|Bridge to [Circe](https://circe.github.io/circe/) encoding/decoding.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-circe/_latestVersion)|
| **Circe YAML**|Bridge to [Circe](https://circe.github.io/circe/) YAML encoding/decoding.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-circe/_latestVersion)|
| **Refined**|Support for [Refined](https://github.com/fthomas/refined) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-refined/_latestVersion)|
| **AWS**|Support for [AWS](https://aws.amazon.com/sdk-for-java/) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-aws/_latestVersion)|
| **Prometheus**|Support for encoding data as [Prometheus](https://prometheus.io) metrics.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-metrics-prometheus/_latestVersion)|
| **Dropwizard**|Support for encoding data as [Dropwizard](https://metrics.dropwizard.io) metrics.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-metrics-dropwizard/_latestVersion)|
| **Spectator**|Support for encoding data as [Spectator](https://github.com/Netflix/spectator) metrics.|[ ![Download](https://api.bintray.com/packages/janstenpickle/extruder/io.extruder/images/download.svg) ](https://bintray.com/janstenpickle/extruder/extruder-metrics-spectator/_latestVersion)|

## Install with SBT
Add the following to your `build.sbt`:
```scala
libraryDependencies += "io.extruder" %% "extruder" % "0.10.0"

// only if you require support for cats-effect instances
libraryDependencies += "io.extruder" %% "extruder-cats-effect" % "0.10.0"

// only if you require support for Typesafe config
libraryDependencies += "io.extruder" %% "extruder-typesafe" % "0.10.0"

// only if you require support for Circe types
libraryDependencies += "io.extruder" %% "extruder-circe" % "0.10.0"

// only if you require support for Circe YAML
libraryDependencies += "io.extruder" %% "extruder-circe-yaml" % "0.10.0"

// only if you require support for refined types
libraryDependencies += "io.extruder" %% "extruder-refined" % "0.10.0"

// only if you require support for AWS config
libraryDependencies += "io.extruder" %% "extruder-aws" % "0.10.0"

// only if you require support for prometheus encoders
libraryDependencies += "io.extruder" %% "extruder-metrics-prometheus" % "0.10.0"

// only if you require support for dropwizard encoders
libraryDependencies += "io.extruder" %% "extruder-metrics-dropwizard" % "0.10.0"

// only if you require support for spectator encoders
libraryDependencies += "io.extruder" %% "extruder-metrics-spectator" % "0.10.0"
```

# Participation

This project supports the Typelevel [code of conduct](http://typelevel.org/conduct.html) and aims that its channels
(mailing list, Gitter, github, etc.) to be welcoming environments for everyone.
