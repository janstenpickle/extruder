
![Extruder](https://i.imgur.com/yFYmS5q.jpg)

# Extruder

[![Build Status](https://travis-ci.org/janstenpickle/extruder.svg?branch=master)](https://travis-ci.org/janstenpickle/extruder)  [ ![Download](https://api.bintray.com/packages/janstenpickle/maven/extruder/images/download.svg) ](https://bintray.com/janstenpickle/maven/extruder/_latestVersion)

This library uses [shapeless](https://github.com/milessabin/shapeless) and [cats](https://github.com/typelevel/cats) to provide a neat syntax to instantiate Scala case classes from a configuration source. 

**Rules for configuration resolution are specified in the declaration of the case class itself:**

```scala
case class ApplicationConfig(default: Int = 100, noDefault: String, optional: Option[Double])

val config: ValidatedNel[ValidationFailure, ApplicationConfig] = resolve[ApplicationConfig](Resolvers)

```

In `ApplicationConfig` above `default` will be set to `100`, `noDefault` will cause a validation failure to be logged and `optional` will be set to `None`, should the configuration source not contain a value for each parameter.

#Contents of This Readme

- [Motivation](#motivation)
- [Quick Start Guide](#quick-start-guide)
- [Extending](#extending)
  - [Resolving Configuration](#resolving-configuration)

#Motivation

Learn more about [shapeless](https://github.com/milessabin/shapeless) having read Dave Gurnell's excellent introduction: ["The Type Astronaut's Guide to Shapeless"](http://underscore.io/books/shapeless-guide/).

Try out [Grafter](https://github.com/zalando/grafter). This project complements applications which use [Grafter](https://github.com/zalando/grafter) or other dependency injection frameworks and techniques by providing a way of resolving values in case classes from a configuration source.

Specifically Grafter requires that all configuration be part single case class to be passed to the entry point of the application. Structuring the config in classes like this works well, but leaves the question of how are these classes populated with config?

This is where Extruder comes in, the [example here](examples/src/main/scala/extruder/examples/Grafter.scala) shows how they may be used together.

#Quick Start Guide

##Install with SBT
Add the following to your sbt `project/plugins.sbt` file:
```scala
addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")
```
Then add the following to your `build.sbt`
```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "extruder" %% "extruder" % "0.0.1"
```

##Simple Case Class

```scala
import extruder.core.SystemPropertiesResolvers
import extruder.resolution._

object Main extends App {
  case class Example(defaultedString: String = "default", configuredString: String, optionalString: Option[String])
  
  println(resolve[Example](SystemPropertiesResolvers)) // Invalid(NonEmptyList(ValidationFailure("Could not find configuration at 'example.configuredstring' and no default available", None)))
  
  System.setProperty("example.configuredstring", "configured")
  println(resolve[Example](SystemPropertiesResolvers)) // Valid(Example("default", "configured", None))
   
  System.setProperty("example.optionalsting", "optional")
  println(resolve[Example](SystemPropertiesResolvers)) // Valid(Example("default", "configured", Some("optional"))) 
}
```

##Nested Case Classes

```scala
import extruder.core.SystemPropertiesResolvers
import extruder.resolution._

object Main extends App {
  case class Example(a: NestedOne, b: NestedTwo)
  case class NestedOne(value: String, nested: NestedTwo)
  case class NestedTwo(value: String)
  
  System.setProperty("example.a.nestedone.value", "nested-one")
  System.setProperty("example.a.nestedone.nested.nestedtwo.value", "nested-one-nested-two")
  System.setProperty("example.b.nestedtwo.value", "nested-two")
  
  // Resolve separate values depending on path to the case class from the top level case class
  println(resolve[Example](SystemPropertiesResolvers)) // Valid(Example(NestedOne("nested-one", NestedTwo("nested-one-nested-two")), NestedTwo("nested-two"))
  
  
  System.setProperty("nestedone.value", "nested-one")
  System.setProperty("nestedtwo.value", "nested-two")
  // Resolve the same values for all instances of the same case class
  
  println(resolve[Example].singletons(SystemPropertiesResolvers)) // Valid(Example(NestedOne("nested-one", NestedTwo("nested-two")), NestedTwo("nested-two"))  
}
```

###Important Caveats

**No cyclical references**
```scala
case class Example(e: Example) 

resolve[Example] // won't compile

case class NestedOne(n: NestedTwo)
case class NestedTwo(n: NestedOne)

resolve[NestedOne] // won't compile
```

#Extending

##Resolving Configuration

The core project ships with a [`Resolvers`](core/src/main/scala/extruder/core/Resolvers.scala) trait and an implementation which uses Java system properties as a configuration source, called `SystemPropertiesResolvers` and shown in the examples above.

The `Resolvers` trait is responsible for providing a set of implicit `Resolver` instances for primitive Scala types. These resolvers are used during the case class construction in [`Extruder`](core/src/main/scala/extruder/core/Extruder.scala), if a resolver cannot be found for a certain type then the compiler will error.

###Implementing a new Set of Resolvers

The `Resolvers` trait assumes your configuration source will simply be providing string values and includes `Resolver` implementations for most Scala primitives, based on the [Mouse library's](https://github.com/benhutchison/mouse) string parsing. Therefore you only have to implement two methods to add a new simple config source:

```scala
object MyResolvers extends Resolvers {
  override def pathToString(path: Seq[String]): String = ???
  override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = ???
  override def lookupList(path: Seq[String]): ConfigValidation[Option[List[String]]] = ???
}
```    

Notice both methods accept the parameter `path` which is a `Seq[String]`, this is essentially the long name of the configuration you are trying to resolve. It is made up of the name of the case class and the name of the parameter.

For example `case class Example(a: String, b: Int)` will evaluate to the paths `Seq("Example", "a")` and `Seq("Example", "b")`, at this point everything is case sensitive, it is up to the implementer as to whether they want their configuration to base case sensitive or not.

####Example Implementation

What will follow is a break down of the implementation of the [`SystemPropertiesResolver`](core/src/main/scala/extruder/core/SystemPropertiesResolver.scala) provided by the core library.

First create a map from system properties to act as the configuration source, notice that in this implementation we have chosen to be case insensitive by making all the property keys lower case: 

```scala
val props: Map[String, String] = System.getProperties.asScala.toMap.map { case (k, v) => k.toLowerCase -> v }
```

#####Implementing `pathToString`
The `pathToString` method is used internally for meaningful error messages on failure of config resolution, using it in `resolveConfig` is optional.

Again, we're using case insensitivity here so convert the path to lower case.
          
```scala
override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
```                                                                                                                                                                       
#####Implementing `lookupValue`                                                                                                                                                                       
Note the return type of `lookupValue` is `ConfigValidation[Option[String]]` which expands to `cats.data.ValidatedNel[ValidationFailure, Option[String]]`. This allows for any errors in looking up configuration to be handled differently to the configuration value not being present. For example, a connection error to a remote configuration source should be handled as an `InvalidNel[String]`, where the error message is a string. Whereas the configuration value not being present should be an empty `Option[String]`. 

```scala
override def lookupValue(path: Seq[String]): ConfigValidation[Option[String]] = props.get(pathToString(path)).validNel

```

The `.validNel` on the end of the lookup is from the `cats.syntax.validated._` package and simply lifts the result of `props.get(pathToString(path))` into the right of `cats.data.ValidatedNel[String, Option[String]]`.

#####Overriding `lookupList`
_Note that this will override the default implementation in [`Resolvers`](core/src/main/scala/extruder/core/Resolvers.scala), if you're happy with the default implementation and just want to change the separator see [Overriding `listSeparator`](#overriding-listseparator)._

The return type for this function is `ConfigValidation[Option[List[String]]]`, this allows for any errors looking up or converting to a list the value. Some sources may provide list or array types natively, if they don't you may implement it here.

```scala
override def lookupList(path: Seq[String]): ConfigValidation[Option[List[String]]] =
  lookupValue(path).map(_.map(_.split(",").toList.map(_.trim)))
```

As this config source does not provide a means of looking up a list we look up the value using `lookupValue` and turn the string inside `ConfigValidation[Option[String]]` into a list of strings, where each element is separated by a `,`.

It is also possible to add some validation when converting the string value to a list: 

```scala
override def lookupList(path: Seq[String]): ConfigValidation[Option[List[String]]] =
  lookupValue(path).fold(
    _.invalid,
    _.fold[ConfigValidation[Option[List[String]]]](None.validNel)(value => 
      if (value.contains(",")) Some(value.split(",").toList.map(_.trim)).validNel
      else ValidationFailure(
        s"No separator (,) found in value '$value' when attempting to create a list for '${pathToString(path)}'"
      )
    )                    
  )
```

#####Overriding `listSeparator`

If you are happy with the default implementation of `lookupList`, but want to change the list separator then you can simply override `listSeparator` and set it to anything you like.

###Extending an Existing Set of Resolvers

Say you wanted to add a resolver for a certain type it is possible to extend an existing implementation of `Resolvers` to parse the new type. Below is an example adding a new resolver for `URL`: 

```scala
import cats.syntax.either._
import java.net.URL
import extruder.core.SystemPropertiesResolvers
import extruder.core.Parser

class WithURL extends SystemPropertiesResolvers {
  implicit val url: Parser[URL] = value => Either.catchNonFatal(new URL(value))
}

```

#Participation

This project supports the Typelevel [code of conduct](http://typelevel.org/conduct.html) and aims that its channels 
(mailing list, Gitter, github, etc.) to be welcoming environments for everyone.