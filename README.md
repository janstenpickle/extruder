<div style="text-align:center"><img src ="https://janstenpickle.github.io/extruder/img/main.png" /></div>

# Extruder

[![Typelevel Incubator](https://img.shields.io/badge/typelevel-incubator-green.svg)](http://typelevel.org/projects) [![Build Status](https://travis-ci.org/janstenpickle/extruder.svg?branch=master)](https://travis-ci.org/janstenpickle/extruder) [![codecov](https://codecov.io/gh/janstenpickle/extruder/branch/master/graph/badge.svg)](https://codecov.io/gh/janstenpickle/extruder)

This library uses [shapeless](https://github.com/milessabin/shapeless) and [cats](https://github.com/typelevel/cats) to provide a neat syntax to instantiate Scala case classes from a data source.

[See the extruder microsite for detailed documentation.](https://janstenpickle.github.io/extruder/)

# Modules
|Module|Description|Download|
|---|---|---|
| **Extruder**|Main module, includes core functionality and basic resolvers.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder/_latestVersion)|
| **Typesafe Config**|Support for resolution from [Typesafe Config](https://github.com/typesafehub/config).|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-typesafe/_latestVersion)|
| **Refined**|Support for [Refined](https://github.com/fthomas/refined) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-refined/_latestVersion)|
| **Monix**|Support for [Monix](https://monix.io) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-monix/_latestVersion)|
| **FS2**|Support for [FS2](https://github.com/functional-streams-for-scala/fs2) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-fs2/_latestVersion)|


## Install with SBT
Add the following to your `build.sbt`:
```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "extruder" %% "extruder" % "0.6.2"

// only if you require support for Typesafe config
libraryDependencies += "extruder" %% "extruder-typesafe" % "0.6.2"

// only if you require support for refined types
libraryDependencies += "extruder" %% "extruder-refined" % "0.6.2"

// only if you require support for wrapping result in monix's task
libraryDependencies += "extruder" %% "extruder-monix" % "0.6.2"

// only if you require support for wrapping result in fs2's task
libraryDependencies += "extruder" %% "extruder-fs2" % "0.6.2"
```

# Participation

This project supports the Typelevel [code of conduct](http://typelevel.org/conduct.html) and aims that its channels
(mailing list, Gitter, github, etc.) to be welcoming environments for everyone.
