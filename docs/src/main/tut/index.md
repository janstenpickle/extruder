---
layout: home
title:  "Home"
section: "home"
---
<div style="text-align:center"><img src ="{{ site.url }}/extruder/img/extruder.svg" /></div>

# Extruder

[![Typelevel Incubator](https://img.shields.io/badge/typelevel-incubator-green.svg)](http://typelevel.org/projects) [![Build Status](https://travis-ci.org/janstenpickle/extruder.svg?branch=master)](https://travis-ci.org/janstenpickle/extruder)

This library uses [Shapeless](https://github.com/milessabin/shapeless), [Cats](https://github.com/typelevel/cats) and [Cats Effect](https://github.com/typelevel/cats-effect) to provide a neat syntax to instantiate Scala case classes from a data source.

## Quickstart Guide

<a name="quick-start"></a>

{% include_relative quickstart.md %}

## Motivation

To learn more about [shapeless][shapeless] having read Dave Gurnell's excellent introduction: ["The Type Astronaut's Guide to Shapeless"](http://underscore.io/books/shapeless-guide/).

Try out [Grafter][grafter]. This project complements applications which use [Grafter][grafter] or other dependency injection frameworks and techniques by providing a way of resolving values in case classes from a data source.

Specifically Grafter requires that all configuration be part single case class to be passed to the entry point of the application. Structuring the config in classes like this works well, but leaves the question of how are these classes populated with config?

This is where Extruder comes in, you can load your configuration from a [data source](data_source.html) and use it with [Grafter][grafter] for dependency injection.

# Modules

|Module|Description|Download|
|---|---|---|
| **Extruder**|Main module, includes core functionality and basic resolvers.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder/_latestVersion)|
| **Cats Effect**|Provides Cats Effect [typeclass implementations](concepts.html#effects).|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder/_latestVersion)|
| **Typesafe Config**|Support for resolution from [Typesafe Config](https://github.com/typesafehub/config).|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-typesafe/_latestVersion)|
| **Circe**|Bridge to [Circe](https://circe.github.io/circe/) encoding/decoding.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-circe/_latestVersion)|
| **Refined**|Support for [Refined](https://github.com/fthomas/refined) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-refined/_latestVersion)|
| **AWS**|Support for [AWS](https://aws.amazon.com/sdk-for-java/) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-aws/_latestVersion)|
| **Prometheus**|Support for encoding data as [Prometheus](https://prometheus.io) metrics.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-metrics-prometheus/_latestVersion)|
| **Dropwizard**|Support for encoding data as [Dropwizard](https://metrics.dropwizard.io) metrics.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-metrics-dropwizard/_latestVersion)|
| **Spectator**|Support for encoding data as [Spectator](https://github.com/Netflix/spectator) metrics.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-metrics-spectator/_latestVersion)|

# Supported Functionality

- [Parsing of primitive types](usage.html#primitive-types):
{% include primitives.md %}
- [Support for refined types](refined.html)
- [Case class resolution](usage.html#simple-case-class)
- [Sealed type member resolution (ADTs)](usage.html#sealed-type-families)
- Resolution from multiple data sources:
  - [Simple Map (`Map[String, String]`)](core/src/main/scala/extruder/core/Map.scala)
  - [System Properties](core/src/main/scala/extruder/core/SystemPropertiesConfig.scala)
  - [Typesafe Config](typesafe/src/main/scala/extruder/typesafe/TypesafeConfig.scala)
- [Pluggable data backends](data_sources.html)
- [Addition of more types](extending.html)

# Similar Projects

### PureConfig
[PureConfig][pureconfig] uses a similar technique to create case classes from [Typesafe Config][typesafe].


- Extruder supports pluggable backends - does not rely on [Typesafe Config][typesafe] by default
- Pureconfig supports time parsing using `java.time` which Extruder does not out of the box, a date parsing module may be added in the future, until then [custom data sources may be added](#extending-an-existing-set-of-resolvers)
- Resolution of Typesafe `ConfigValue`, `ConfigObject` and `ConfigList` is only supported by the Typesafe Config configuration backend, as it is closely tied to Typesafe Config, Pureconfig supports this by default as it is directly tied to Typesafe config
- Pureconfig supports returning `Map[String, String]`, for the time being Extruder does not support this
- Extruder supports control of class and parameter name formatting by [overriding a method](#implementing-pathtostring), however Pureconfig supports this via predefined configuration schemes

# Participation

This project supports the Typelevel [code of conduct](http://typelevel.org/conduct.html) and aims that its channels
(mailing list, Gitter, github, etc.) to be welcoming environments for everyone.

# Licence

#### MIT License

*Copyright (c) 2017-2019 Chris Jansen*

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

{% include references.md %}
