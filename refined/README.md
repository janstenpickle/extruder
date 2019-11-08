# Refined
This module intoduces support for [refined](https://github.com/fthomas/refined) primtive types types.

## Usage
First include the library in your SBT configuration:
```
libraryDependencies += "io.extruder" %% "extruder-refined" % "0.11.0"
```
Then import the refined package and use refined types as you normally would:
```scala
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import extruder.core.MapConfig._
import extruder.refined._

object Main extends App {
  case class Test(positiveInt: Int Refined Positive)

  val goodConfig = Map("test.positiveint" -> "23")
  val badConfig = Map("test.positiveint" -> "-432")

  println(decode[Test](goodConfig)) // Valid(Test(23))

  println(decode[Test](badConfig)) // Invalid(NonEmptyList(ValidationException(Could not parse value '-432' at 'test.positiveint': Predicate failed: (-432 > 0).)))
}
```
