---
layout: docs
title:  "Extending an Existing Data Source"
position: 4
---
## Extending an Existing Data Source

Say you wanted to add a resolver for a certain type it is possible to extend an existing implementation of a data source to parse the new type. Below is an example adding a new decoder for `URL`:

```tut:silent
import cats.effect.IO
import cats.syntax.either._
import cats.syntax.validated._
import java.net.URL
import extruder.core._

trait WithURL extends MapSource {
  implicit def urlDecoder[F[_], E](implicit F: DecEff[F], error: ExtruderErrors[F]): MapDecoder[F, URL] =
    mkDecoder[F, URL]((path, settings, default, src) =>
      (src.get(settings.pathToString(path)), default) match {
        case (Some(v), _) => Either.catchNonFatal(new URL(v)).fold(ex =>
            error.validationException(ex.getMessage, ex),
            F.pure
          )
        case (None, Some(url)) => F.pure(url)
        case _ => error.missing(s"Could not find value for ${settings.pathToString(path)}")
      }
    )

  implicit def urlEncoder[F[_], E](implicit F: EncEff[F]): MapEncoder[F, URL] =
    mkEncoder[F, URL]((path, settings, value) =>
      F.pure(Map(settings.pathToString(path) -> value.toString))
    )
}

object WithURL extends WithURL
```

This is a fairly verbose implementation which repeats some of the abstracted functionality found in `PrimitiveDecoders` and `PrimitiveEncoders`, it must also be implemented for each data type.

This functionality can be implemented more simply and so that it works for all backends which implement the `PrimitiveDecoders` and `PrimitiveEncoders` traits:

```tut:silent
import cats.syntax.either._
import java.net.URL
import extruder.core._

object WithURL {
  implicit val urlParser: Parser[URL] = Parser.fromEitherException(url =>
    Either.catchNonFatal(new URL(url))
  )

  implicit val urlShow: Show[URL] = Show.make(_.toString)
}
```

The object `WithURL` may then be imported wherever extruder is used and being to provide support for the new type.
