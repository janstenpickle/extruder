---
layout: docs
title:  "Usage"
position: 2
---
* TOC
{:toc}
# Supported Functionality

Below are examples of encoding and decoding case classes using the map data source.

Each implementation of a data source may provide differing implementations of `encode` and `decode` methods. However all must implement the most basic methods for decoding and encoding:

- **Decode** which takes a type parameter of the expected type to be decoded, an optional namespace argument and the data source, returning the validated type.

- **Encode** which takes an optional namespace argument and the value to be encoded, returning the value encoded as the data type.

In this section the default return type of `Validation` which is a type alias for the [cats] `ValidatedNel` applicative functor. See the [section on decoding and encoding](decode_encode.html) for information of how to specify different monads.

## Primitive Types
Provided there is a [`Decoder`](concepts.html#Terms) instance for the type you wish to decode, or an [`Encoder`](concepts.html#Terms) instance you wish to encode compilation will work. [It is possible to extend existing data sources if you wish to add support for more types](extending.html).

The following primitive types are provided by the [`PrimitiveDecoders`](https://github.com/janstenpickle/extruder/blob/master/core/src/main/scala/extruder/core/PrimitiveDecoders.scala) and [`PrimitiveEncoders`](https://github.com/janstenpickle/extruder/blob/master/core/src/main/scala/extruder/core/PrimitiveEncoders.scala) classes.

{% include primitives.md %}

The following code shows how an integer value may be decoded and encoded from the [map data source](https://github.com/janstenpickle/extruder/blob/master/core/src/main/scala/extruder/core/Map.scala).

```tut:silent
import extruder.map._

decode[Int](List("some", "int"), Map("some.int" -> "23"))

encode[Int](List("some", "int"), 23)
```

## Simple Case Class
When encoding or decoding a case class the name of the case class is automatically included in the namespace, so there is not always a need to provide one:

```tut:silent
import extruder.map._

case class Example(defaultedString: String = "default", configuredString: String, optionalString: Option[String])

// Fails to decode
decode[Example](Map.empty[String, String])

// Decodes with empty option
decode[Example](Map("example.configuredstring" -> "configured"))

// Decodes with provided option
decode[Example](Map("example.configuredstring" -> "configured", "example.optionalsting" -> "optional"))

// Decodes with a namespace
decode[Example](List("name", "space"), Map("name.space.example.configuredstring" -> "configured"))

// Encodes case class to Map
encode[Example](Example(configuredString = "configured", optionalString = None))

// Encodes case class to Map with namespace
encode[Example](List("name", "space"), Example(configuredString = "configured", optionalString = None))
```

## Nested Case Classes

Case classes may be nested within on another. The key in the data must contain the complete path to the final primitive value:

```tut:silent
import extruder.map._

case class NestedTwo(value: String)
case class NestedOne(value: String, nested: NestedTwo)
case class Example(a: NestedOne, b: NestedTwo, c: Int)

val config = Map(
  "example.a.nestedone.value" -> "nested-one",
  "example.a.nestedone.nested.nestedtwo.value" -> "nested-one-nested-two",
  "example.b.nestedtwo.value" -> "nested-two",
  "example.c" -> "23"
)

decode[Example](config)
```
## Sealed Type Families
Extruder also supports resolution of sealed type members. In order to pick the implementation of the specified trait to decode a `type` value must be provided:

```tut:silent
import extruder.map._

sealed trait Sealed
case object ExampleObj extends Sealed
case class ExampleCC(a: Int) extends Sealed

decode[Sealed](Map("type" -> "ExampleObj"))
encode[Sealed](ExampleObj)

decode[Sealed](Map("type" -> "ExampleObj", "examplecc.a" -> "1"))
encode[Sealed](ExampleCC(1))

// Sealed families may also be nested

case class Example(a: Sealed)

decode[Example](Map("example.a.type" -> "ExampleObj"))
encode[Example](Example(ExampleObj))

decode[Example](Map("example.a.type" -> "ExampleObj", "example.a.examplecc.a" -> "1"))
encode[Example](Example(ExampleCC(1)))
```

## Changing Key Format
As implied in the above examples the configuration keys are dot (`.`) separated and all lowercase. This is configurable by creating a new instance of `Settings` for the configuration source and passing it to the decode or table representation methods.

```tut:silent
import extruder.core.Settings
import extruder.map._

val settings: Settings = new Settings {
  override def pathToString(path: List[String]): String = path.mkString("_").toUpperCase
}

case class Example(defaultedString: String = "default", configuredString: String, optionalString: Option[String])

println(parameters[Example](settings))
```
Changing the `pathToString` implementation will change the expected configuration keys:
```
+--------------------------+----------+--------+---------+------------------+
| Key                      | Required | Type   | Default | Permitted Values |
+--------------------------+----------+--------+---------+------------------+
| EXAMPLE_DEFAULTEDSTRING  | N        | String | default |                  |
| EXAMPLE_CONFIGUREDSTRING | Y        | String |         |                  |
| EXAMPLE_OPTIONALSTRING   | N        | String |         |                  |
+--------------------------+----------+--------+---------+------------------+
```

The [Utils](https://github.com/janstenpickle/extruder/blob/master/core/src/main/scala/extruder/core/Utils.scala) code shows what else may be overridden.

# Unsupported Functionality

**Cyclical references**
```scala
import extruder.system.systemproperties._

case class Example(e: Example)

decode[Example] // won't compile

case class NestedOne(n: NestedTwo)
case class NestedTwo(n: NestedOne)

decode[NestedOne] // won't compile
```
{% include references.md %}
