#### Follow the instructions below or [try it out in Scastie!](https://scastie.scala-lang.org/janstenpickle/ozr3LrFpRdyDUqXio3RGtA/1)

Add the following to your `build.sbt`:
```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "extruder" %% "extruder" % "0.6.0"

// only if you require support for Typesafe config
libraryDependencies += "extruder" %% "extruder-typesafe" % "0.6.0"

// only if you require support for refined types
libraryDependencies += "extruder" %% "extruder-refined" % "0.6.0"

// only if you require support for wrapping result in monix's task
libraryDependencies += "extruder" %% "extruder-monix" % "0.6.0"

// only if you require support for wrapping result in fs2's task
libraryDependencies += "extruder" %% "extruder-fs2" % "0.6.0"
```

**Rules for configuration resolution are specified in the declaration of the case class itself:**

In `ApplicationConfig` below `default` will be set to `100`, `noDefault` will cause a validation failure to be logged and `optional` will be set to `None`, should the configuration source not contain a value for each parameter:

```tut:silent
import cats.data.ValidatedNel
import cats.effect.IO
import extruder.core._
import extruder.core.MapConfig._
import extruder.monix._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Future

case class ApplicationConfig(default: Int = 100, noDefault: String, optional: Option[Double])

val config: Map[String, String] = Map("applicationconfig.nodefault" -> "extruder")
val applicationConfig: ApplicationConfig = ApplicationConfig(noDefault = "extruder", optional = None)

// Decode from configuration into different target monads
val decoded: ValidatedNel[ValidationError, ApplicationConfig] = decode[ApplicationConfig](config)
val decodedEither: Either[ValidationErrors, ApplicationConfig] =
  decode[ApplicationConfig, EitherErrors, ValidationErrors](config)
val decodedTask: Task[ApplicationConfig] = decode[ApplicationConfig, Task, Throwable](config)
val decodedIO: IO[ValidatedNel[ValidationError, ApplicationConfig]] = decodeIO[ApplicationConfig](config)
val decodedAsync: Future[ValidatedNel[ValidationError, ApplicationConfig]] =
  decodeAsync[ApplicationConfig, Future](config)
val decodedAsyncEither: Future[Either[ValidationErrors, ApplicationConfig]] =
  decodeAsync[ApplicationConfig, Future, EitherErrors, ValidationErrors](config)

// Encode to configuration into different target monads
val encoded: ValidatedNel[ValidationError, Map[String, String]] = encode(applicationConfig)
val encodedEither: Either[ValidationErrors, Map[String, String]] =
  encode[ApplicationConfig, EitherErrors, ValidationErrors](applicationConfig)
val encodedTask: Task[Map[String, String]] = encode[ApplicationConfig, Task, Throwable](applicationConfig)
val encodedIO: IO[ValidatedNel[ValidationError, Map[String, String]]] = encodeIO(applicationConfig)
val encodedAsync: Future[ValidatedNel[ValidationError, Map[String, String]]] =
  encodeAsync[ApplicationConfig, Future](applicationConfig)
val encodedAsyncEither: Future[Either[ValidationErrors, Map[String, String]]] =
  encodeAsync[ApplicationConfig, Future, EitherErrors, ValidationErrors](applicationConfig)

// Show configuration parameters as a table
val params: String = parameters[ApplicationConfig]
println(params)
```
Printing configuration parameters outputs the following:
```
+-----------------------------+----------+--------+---------+------------------+
| Key                         | Required | Type   | Default | Permitted Values |
+-----------------------------+----------+--------+---------+------------------+
| applicationconfig.default   | N        | Int    | 100     |                  |
| applicationconfig.nodefault | Y        | String |         |                  |
| applicationconfig.optional  | N        | Double |         |                  |
+-----------------------------+----------+--------+---------+------------------+
```
