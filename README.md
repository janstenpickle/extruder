
# Shape Sorter

[![Build Status](https://travis-ci.org/janstenpickle/shapeless-config.svg?branch=master)](https://travis-ci.org/janstenpickle/shapeless-config)

This library uses [shapeless](https://github.com/milessabin/shapeless) and [cats](https://github.com/typelevel/cats) to provide a neat syntax to instantiate Scala case classes from a configuration source. 

**Rules for configuration resolution are encoded in the declaration of the case class itself**

```scala
case class ApplicationConfig(default: Int = 100, noDefault: String, optional: Option[Double])

val config: ValidatedNel[ValidationFailure, ApplicationConfig] = resolve[ApplicationConfig](Resolvers)

```

In `ApplicationConfig` above `default` will be set to `100`, `noDefault` will cause a validation failure to be logged and `optional` will be set to None, should the configuration source not contain a value for each parameter.

## Motivation

Shape Sorter complements applications which use [Grafter](https://github.com/zalando/grafter) or other dependency injection frameworks or patterns.

Specifically Grafter requires that all configuration be part single case class to be passed to the entry point of the application. Structuring the config in classes like this works well, but leaves the question of how are these classes populated with config?

This is where Shape Sorter comes in, the [example here](examples/src/main/scala/shapelessconfig/examples/Grafter.scala) shows how they may be used together.

## Quick Start Guide

### Install with SBT
Add the following to your sbt `project/plugins.sbt` file:
```scala
addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")
```
Then add the following to your `build.sbt`
```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "shapelessconfig" %% "shaplessconfig" % "0.1.0"
```

###Simple Case Class

```scala
import shapelessconfig.core.SystemPropertiesResolvers
import shapelessconfig.macros.resolution._

object Main extends App {
  case class Example(defaultedString: String = "default", configuredString: String, optionalString: Option[String])
  
  println(resolve[Example](SystemPropertiesResolvers)) // Invalid(NonEmptyList(ValidationFailure("Could not find configuration at 'example.configuredstring' and no default available", None)))
  
  System.setProperty("example.configuredstring", "configured")
  println(resolve[Example](SystemPropertiesResolvers)) // Valid(Example("default", "configured", None))
   
  System.setProperty("example.optionalsting", "optional")
  println(resolve[Example](SystemPropertiesResolvers)) // Valid(Example("default", "configured", Some("optional"))) 
}
```

###Nested Case Classes

```scala
import shapelessconfig.core.SystemPropertiesResolvers
import shapelessconfig.macros.resolution._

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

####Important Caveats

**No cyclical references**
```scala
case class Example(e: Example) 

resolve[Example] // won't compile

case class NestedOne(n: NestedTwo)
case class NestedTwo(n: NestedOne)

resolve[NestedOne] // won't compile
```

## Resolving Configuration

The main project ships with a [`Resolvers`](core/src/main/scala/shapelessconfig/core/Resolvers.scala) trait and an implementation which uses Java system properties as a configuration source, called `SystemPropertiesResolvers` and shown in the examples above.

The `Resolvers` trait is responsible for providing a set of implicit `Resolver` instances for primitive Scala types. These resolvers are used during the case class construction in [`ConfigConstructor`](core/src/main/scala/shapelessconfig/core/ConfigConstructor.scala), if a resolver cannot be found for a certain type then the compiler will error.

###Implementing a new Set of Resolvers

The `Resolvers` trait assumes your configuration source will simply be providing string values and includes `Resolver` implementations for most Scala primitives, based on the [Mouse library's](https://github.com/benhutchison/mouse) string parsing. Therefore you only have to implement two methods to add a new simple config source:

```scala
object MyResolvers extends Resolvers {
  override def pathToString(path: Seq[String]): String = ???
  override def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]] = ???
}
```    

Notice both methods accept the parameter `path` which is a `Seq[String]`, this is essentially the long name of the configuration you are trying to resolve. It is made up of the name of the case class and the name of the parameter.

For example `case class Example(a: String, b: Int)` will evaluate to the paths `Seq("Example", "a")` and `Seq("Example", "b")`, at this point everything is case sensitive, it is up to the implementer as to whether they want their configuration to base case sensistive or not.

####Example Implementation

What will follow is a break down of the implementation of the [`SystemPropertiesResolver`](core/src/main/scala/shapelessconfig/core/SystemPropertiesResolver.scala) provided by the core library.

First create a map from system properties to act as the configuration source, notice that in this implementation I have chosen to be case insensitive by making all the property keys lower case: 

```scala
val props: Map[String, String] = System.getProperties.asScala.toMap.map { case (k, v) => k.toLowerCase -> v }
```

#####Implementing `pathToString`
The `pathToString` method is used internally for meaningful error messages on failure of config resolution, using it in `resolveConfig` is optional.

Again, I'm using case insensitivity here so convert the path to lower case.
          
```scala
override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
```                                                                                                                                                                       
#####Implementing `resolveConfig`                                                                                                                                                                       
Note the return type of `resolveConfig` is `ConfigValidation[Option[String]]` which expands to `cats.data.ValidatedNel[ValidationFailure, Option[String]]`. This allows for any errors in looking up configuration to be handled differently to the configuration value not being present. For example, a connection error to a remote configuration source should be handled as an `InvalidNel[String]`, where the error message is a string. Whereas the configuration value not being present should be an empty `Option[String]`. 

```scala
override def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]] = props.get(pathToString(path)).validNel

```

The `.validNel` on the end of the lookup is from the `cats.syntax.validated._` package and simply lifts the result of `props.get(pathToString(path))` into the right of `cats.data.ValidatedNel[String, Option[String]]`.


###Extending an Existing Set of Resolvers

Say you wanted to add a resolver for a certain type it is possible to extend an existing implementation of `Resolvers` to parse the new type. Below is an example adding a new resolver for `URL`: 

```scala
import cats.syntax.either._
import java.net.URL
import shapelessconfig.core.SystemPropertiesResolvers
import shapelessconfig.core.Resolver

class WithURL extends SystemPropertiesResolvers {
  implicit val url: Resolver[URL] = Resolver(resolveEither(value => Either.catchNonFatal(new URL(value))))
}

```