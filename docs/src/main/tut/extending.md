---
layout: docs
title:  "Extending an Existing Config Source"
position: 4
---
## Extending an Existing Config Source

Say you wanted to add a resolver for a certain type it is possible to extend an existing implementation of a configuration source to parse the new type. Below is an example adding a new decoder for `URL`:

```tut:silent
import cats.effect.IO
import cats.syntax.either._
import cats.syntax.validated._
import java.net.URL
import extruder.core._

trait WithURL extends MapConfig {
  implicit def urlDecoder[F[_], E](implicit hints: Hint,
                                   AE: ExtruderApplicativeError[F, E]): MapDecoder[F, URL] =
    mkDecoder[F, URL]((path, default, config) => IO {
      (config.get(hints.pathToString(path)), default) match {
        case (Some(v), _) => Either.catchNonFatal(new URL(v)).fold(ex =>
            AE.validationException(ex.getMessage, ex),
            AE.pure
          )
        case (None, Some(url)) => AE.pure(url)
        case _ => AE.missing(s"Could not find value for ${hints.pathToString(path)}")
      }
    })

  implicit def urlEncoder[F[_], E](implicit hints: Hint,
                                   AE: ExtruderApplicativeError[F, E]): MapEncoder[F, URL] =
    mkEncoder[F, URL]((path, value) =>
      IO(AE.pure(Map(hints.pathToString(path) -> value.toString)))
    )
}

object WithURL extends WithURL
```

This is a fairly verbose implementation which repeats some of the abstracted functionality found in `PrimitiveDecoders` and `PrimitiveEncoders`, it must also be implemented for each configuration type.

This functionality can be implemented more simply and so that it works for all configuration backends which implement the `PrimitiveDecoders` and `PrimitiveEncoders` traits:

```tut:silent
import cats.syntax.either._
import java.net.URL
import extruder.core._

object WithURL {
  implicit val urlParser: Parser[URL] = Parser.fromEitherException(url =>
    Either.catchNonFatal(new URL(url))
  )

  implicit val urlShow: Show[URL] = new Show(_.toString)
}
```

The object `WithURL` may then be imported wherever extruder is used and being to provide support for the new type.
