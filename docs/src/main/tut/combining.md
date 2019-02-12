---
layout: docs
title:  "Combining Data Sources"
position: 8
---

Extruder has the ability to combine two of more data sources which fall back when decoding and write to multiple places on encoding. This can be achieved by chaining `.combine` when calling `encode` or `decode`.

There are multiple ways to call `combine`: by passing another data source, another decode or encode call, or by specifying type parameters. Below is an example of using another data source to encode and decode.

```tut:silent
import extruder.map._
import extruder.circe.instances._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._

case class Example(configuredString: String, optionalString: Option[String])

val example = Example("test", None)

val encoder = encode.combine(extruder.circe.datasource)

encode.combine(extruder.circe.datasource)(example) // Ior.Both(Map(example.configuredstring -> test),{ "configuredString" : "test",  "optionalString" : null })

val map = Map("example.configuredstring" -> "test")
val json = Map("configuredString" -> "test").asJson

val decoder = decode[Example].combine(extruder.circe.datasource)

decoder((Map.empty[String, String], json)) == decoder((map, Json.Null)) // true
```

For usage information of the various `combine` methods see the API documentation for the various partially applied encoders and decoders:

* [`EncodePartiallyApplied`](api/extruder/core/EncodePartiallyApplied.html)
* [`EncodePartiallyAppliedWithDefaultSettings`](api/extruder/core/EncodePartiallyAppliedWithDefaultSettings.html)
* [`DecodePartiallyApplied`](api/extruder/core/DecodePartiallyApplied.html)
* [`DecodePartiallyAppliedWithDefaultSettings`](api/extruder/core/DecodePartiallyAppliedWithDefaultSettings.html)