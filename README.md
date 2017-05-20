![Extruder](https://i.imgur.com/yFYmS5q.jpg)

# Extruder

[![Typelevel Incubator](https://img.shields.io/badge/typelevel-incubator-green.svg)](http://typelevel.org/projects) [![Build Status](https://travis-ci.org/janstenpickle/extruder.svg?branch=master)](https://travis-ci.org/janstenpickle/extruder) [![Coverage Status](https://coveralls.io/repos/github/janstenpickle/extruder/badge.svg?branch=master)](https://coveralls.io/github/janstenpickle/extruder?branch=master)

This library uses [shapeless](https://github.com/milessabin/shapeless) and [cats](https://github.com/typelevel/cats) to provide a neat syntax to instantiate Scala case classes from a configuration source.

**Rules for configuration resolution are specified in the declaration of the case class itself:**
```scala
import extruder.system.SystemPropertiesConfig._

case class ApplicationConfig(default: Int = 100, noDefault: String, optional: Option[Double])

val config: ValidatedNel[ValidationFailure, ApplicationConfig] = decode[ApplicationConfig]

```

In `ApplicationConfig` above `default` will be set to `100`, `noDefault` will cause a validation failure to be logged and `optional` will be set to `None`, should the configuration source not contain a value for each parameter.

# Contents of This Readme

- [Modules](#modules)
- [Motivation](#motivation)
- [Supported Functionality](#supported-functionality)
- [Unsupported Functionality](#supported-functionality)
- [Similar Projects](#similar-projects)
- [Quick Start Guide](#quick-start-guide)
- [Extending](#extending)
- [Participation](#participation)

# Modules
|Module|Description|Download|
|---|---|---|
|**Extruder**|Main module, includes core functionality and basic resolvers.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder/_latestVersion)|
|**Typesafe Config**|Support for resolution from [Typesafe Config](https://github.com/typesafehub/config).|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-typesafe/_latestVersion)|
|**Refined**|Support for [refined](https://github.com/fthomas/refined) types.|[ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder-refined/_latestVersion)|


## Install with SBT
Add the following to your `build.sbt`:
```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "extruder" %% "extruder" % "0.4.0"

// only if you require support for Typesafe config
libraryDependencies += "extruder" %% "extruder-typesafe" % "0.4.0"

// only if you require support for refined types
libraryDependencies += "extruder" %% "extruder-refined" % "0.4.0"
```

# Motivation

To learn more about [shapeless](https://github.com/milessabin/shapeless) having read Dave Gurnell's excellent introduction: ["The Type Astronaut's Guide to Shapeless"](http://underscore.io/books/shapeless-guide/).

Try out [Grafter](https://github.com/zalando/grafter). This project complements applications which use [Grafter](https://github.com/zalando/grafter) or other dependency injection frameworks and techniques by providing a way of resolving values in case classes from a configuration source.

Specifically Grafter requires that all configuration be part single case class to be passed to the entry point of the application. Structuring the config in classes like this works well, but leaves the question of how are these classes populated with config?

This is where Extruder comes in, the [example here](examples/src/main/scala/extruder/examples/Grafter.scala) shows how they may be used together.


# Supported Functionality

- [Parsing of primitive types](core/src/main/scala/extruder/core/Resolvers.scala):
  - Char
  - String
  - Int
  - Long
  - Double
  - Float
  - Short
  - Byte
  - Boolean
  - URL
  - Duration
  - Finite Duration
- [Support for refined types](refined/)
- [Case class resolution](#simple-case-class)
- [Sealed type member resolution (ADTs)](#sealed-type-families)
- Resolution from multiple configuration sources:
  - [Simple Map (`Map[String, String]`)](core/src/main/scala/extruder/core/Map.scala)
  - [System Properties](core/src/main/scala/extruder/core/SystemPropertiesConfig.scala)
  - [Typesafe Config](typesafe/src/main/scala/extruder/typesafe/TypesafeConfig.scala)
- [Pluggable configuration backends](#extending)
- [Addition of more types](#extending-an-existing-config-source)

# Unsupported Functionality

**Cyclical references**
```scala
import extruder.system.SystemPropertiesConfig._

case class Example(e: Example)

decode[Example] // won't compile

case class NestedOne(n: NestedTwo)
case class NestedTwo(n: NestedOne)

decode[NestedOne] // won't compile
```

# Similar Projects

### PureConfig
[PureConfig](https://github.com/melrief/pureconfig) uses a similar technique to create case classes from [Typesafe Config](https://github.com/typesafehub/config).


- Extruder accumilates configuration resolution errors, so a single invocation will highlight all problems with the configuration source
- Extruder supports pluggable configuration backends - does not rely on [Typesafe Config](https://github.com/typesafehub/config) by default
- Pureconfig supports time parsing using `java.time` which Extruder does not out of the box, a date parsing module may be added in the future, until then [custom configuration sources may be added](#extending-an-existing-set-of-resolvers)
- Resolution of Typesafe `ConfigValue`, `ConfigObject` and `ConfigList` is only supported by the Typesafe Config configuration backend, as it is closely tied to Typesafe Config, Pureconfig supports this by default as it is directly tied to Typesafe config
- Pureconfig supports returning `Map[String, String]`, for the time being Extruder does not support this
- Extruder supports control of class and parameter name formatting by [overriding a method](#implementing-pathtostring), however Pureconfig supports this via predefined configuration schemes

# Quick Start Guide

## Simple Case Class

```scala
import extruder.system.SystemPropertiesConfig._

object Main extends App {
  case class Example(defaultedString: String = "default", configuredString: String, optionalString: Option[String])

  println(decode[Example]) // Invalid(NonEmptyList(ValidationFailure("Could not find configuration at 'example.configuredstring' and no default available", None)))

  System.setProperty("example.configuredstring", "configured")
  println(decode[Example]) // Valid(Example("default", "configured", None))

  System.setProperty("example.optionalsting", "optional")
  println(decode[Example]) // Valid(Example("default", "configured", Some("optional")))
}
```

## Nested Case Classes

```scala
import extruder.system.SystemPropertiesConfig._

object Main extends App {
  case class Example(a: NestedOne, b: NestedTwo)
  case class NestedOne(value: String, nested: NestedTwo)
  case class NestedTwo(value: String)

  System.setProperty("example.a.nestedone.value", "nested-one")
  System.setProperty("example.a.nestedone.nested.nestedtwo.value", "nested-one-nested-two")
  System.setProperty("example.b.nestedtwo.value", "nested-two")

  println(decode[Example]) // Valid(Example(NestedOne("nested-one", NestedTwo("nested-one-nested-two")), NestedTwo("nested-two"))
}
```
## Sealed Type Families

```scala
import extruder.system.SystemPropertiesConfig
import extruder.resolution._

object TopLevelSealed extends App {
  sealed trait Sealed
  case object ExamplObj
  case class ExampleCC(a: Int)

  System.setProperty("type", "ExampleObj")

  println(decode[Example]) // Valid(ExampleObj)

  System.setProperty("type", "ExampleCC")
  System.setProperty("examplecc.a", "1")

  println(decode[Example]) // Valid(ExampleCC(1))
}
```

```scala
import extruder.system.SystemPropertiesConfig
import extruder.resolution._

object NestedSealed extends App {
  sealed trait Sealed
  case object ExamplObj
  case class ExampleCC(a: Int)

  case class Example(a: Sealed)

  System.setProperty("example.a.type", "ExampleObj")

  println(decode[Example]) // Valid(Example(ExampleObj))

  System.setProperty("example.a.type", "ExampleCC")
  System.setProperty("example.a.examplecc.a", "1")

  println(decode[Example]) // Valid(Example(ExampleCC(1)))
}

```

# Extending

## Extending an Existing Config Source

Say you wanted to add a resolver for a certain type it is possible to extend an existing implementation of `Resolvers` to parse the new type. Below is an example adding a new decoder for `URL`:

```scala
import cats.syntax.either._
import cats.syntax.validated._
import java.net.URL
import extruder.core.MapConfig
import extruder.core.Parser
import extruder.core.Missing
import extruder.core.ValidationException

trait WithURL extends MapConfig {
  implicit val urlDecoder: MapDecoder[URL] =
    mkDecoder[URL]((path, default, config) =>
      config
      .get(pathToString(path))
      .fold(default)(Some(_))
      .fold[ConfigValidation[URL]](Missing(s"Could not find value for ${pathToString(path)}").invalidNel)(value =>
        Either.catchNonFatal(new URL(value)).leftMap(ex =>
          ValidationException(ex.getMessage, ex)
        ).toValidatedNel
      )
    )

  implicit val urlEncoder: MapEncoder[URL] =
    mkEncoder[URL]((path, value) =>
      Map(pathToString(path) -> value.toString).validNel
    )
}

object WithURL extends WithURL
```

This is a fairly verbose implementation which repeats some of the abstracted functionality found in `PrimitiveDecoders` and `PrimitiveEncoders`, it must also be implemented for each configuration type.

This functionality can be implemented more simply and so that it works for all configuration backends which implement the `PrimitiveDecoders` and `PrimitiveEncoders` traits:

```scala
import cats.syntax.either._
import java.net.URL
import extruder.core.MapConfig
import extruder.core.Parser
import extruder.core.Show

object WithURL {
  implicit val urlParser: Parser[URL] = url => Either.catchNonFatal(new URL(url))

  implicit val urlShow: Show[URL] = Show.fromToString
}
```

The object `WithURL` may then be imported wherever extruder is used and being to provide support for the new type.

# Participation

This project supports the Typelevel [code of conduct](http://typelevel.org/conduct.html) and aims that its channels
(mailing list, Gitter, github, etc.) to be welcoming environments for everyone.
