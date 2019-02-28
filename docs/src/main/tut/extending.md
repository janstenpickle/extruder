---
layout: docs
title:  "Extending an Existing Data Source"
position: 4
---
## Extending an Existing Data Source

Say you wanted to add a resolver for a certain type it is possible to extend an existing implementation of a data source to parse the new type. Below is an example adding a new decoder for `URL`  from a datasource of `Map[String, String]`:

```tut:silent
import cats.Applicative
import cats.effect.IO
import cats.syntax.either._
import cats.syntax.validated._
import java.net.URL
import extruder.core._

trait WithURL {
  implicit def urlMapDecoder[F[_], S <: Settings](implicit F: Applicative[F], error: ExtruderErrors[F]): Decoder[F, S, URL, Map[String, String]] =
    Decoder.make[F, S, URL, Map[String, String]]((path, settings, default, input) =>
      (input.get(settings.pathElementListToString(path)), default) match {
        case (Some(v), _) => error.fromEitherThrowable(Either.catchNonFatal(new URL(v)))
        case (None, Some(url)) => F.pure(url)
        case _ => error.missing(s"Could not find value for ${settings.pathElementListToString(path)}")
      }
    )

  implicit def urlEncoder[F[_], S <: Settings](implicit F: Applicative[F]): Encoder[F, S, URL, Map[String, String]]  =
    Encoder.make[F, S, URL, Map[String, String]]((path, settings, value) =>
      F.pure(Map(settings.pathElementListToString(path) -> value.toString))
    )
}

object WithURL extends WithURL
```

This is a fairly verbose implementation which repeats some of the abstracted functionality found in `ParserDecoderInstances` and `ShowEncoderInstances`, it must also be implemented for each data type.

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
