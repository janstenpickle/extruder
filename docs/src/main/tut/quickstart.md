Add the following to your `build.sbt`:
```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "extruder" %% "extruder" % "0.5.0"

// only if you require support for Typesafe config
libraryDependencies += "extruder" %% "extruder-typesafe" % "0.5.0"

// only if you require support for refined types
libraryDependencies += "extruder" %% "extruder-refined" % "0.5.0"
```

**Rules for configuration resolution are specified in the declaration of the case class itself:**
```tut:silent
import cats.data.ValidatedNel
import extruder.core.ValidationError
import extruder.system.SystemPropertiesConfig._

case class ApplicationConfig(default: Int = 100, noDefault: String, optional: Option[Double])

val config: ValidatedNel[ValidationError, ApplicationConfig] = decode[ApplicationConfig]
```

In `ApplicationConfig` above `default` will be set to `100`, `noDefault` will cause a validation failure to be logged and `optional` will be set to `None`, should the configuration source not contain a value for each parameter.
