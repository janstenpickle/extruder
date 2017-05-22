---
layout: docs
title:  "Extending an Existing Config Source"
position: 2
---
## Extending an Existing Config Source

Say you wanted to add a resolver for a certain type it is possible to extend an existing implementation of a configuration source to parse the new type. Below is an example adding a new decoder for `URL`:

```tut:silent
import cats.syntax.either._
import cats.syntax.validated._
import java.net.URL
import extruder.core.ConfigValidation
import extruder.core.MapConfig
import extruder.core.MapDecoder
import extruder.core.MapEncoder
import extruder.core.Parser
import extruder.core.Missing
import extruder.core.ValidationException

trait WithURL extends MapConfig {
  implicit val urlDecoder: MapDecoder[URL] =
    mkDecoder[URL]((path, default, config) =>
      (config.get(utils.pathToString(path)), default) match {
        case (Some(v), _) => Either.catchNonFatal(new URL(v)).leftMap(ex =>
            ValidationException(ex.getMessage, ex)
          ).toValidatedNel
        case (None, Some(url)) => url.validNel
        case _ => Missing(s"Could not find value for ${utils.pathToString(path)}").invalidNel
      }
    )

  implicit val urlEncoder: MapEncoder[URL] =
    mkEncoder[URL]((path, value) =>
      Map(utils.pathToString(path) -> value.toString).validNel
    )
}

object WithURL extends WithURL
```

This is a fairly verbose implementation which repeats some of the abstracted functionality found in `PrimitiveDecoders` and `PrimitiveEncoders`, it must also be implemented for each configuration type.

This functionality can be implemented more simply and so that it works for all configuration backends which implement the `PrimitiveDecoders` and `PrimitiveEncoders` traits:

```tut:silent
import cats.syntax.either._
import java.net.URL
import extruder.core.MapConfig
import extruder.core.Parser
import extruder.core.Show

object WithURL {
  implicit val urlParser: Parser[URL] = Parser.fromEitherException(url =>
    Either.catchNonFatal(new URL(url))
  )

  implicit val urlShow: Show[URL] = new Show(_.toString)
}
```

The object `WithURL` may then be imported wherever extruder is used and being to provide support for the new type.
