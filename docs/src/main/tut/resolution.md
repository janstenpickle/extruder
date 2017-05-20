---
layout: docs
title:  "Resolving Configuration"
position: 2
---
## Simple Case Class

```tut:silent
import extruder.system.SystemPropertiesConfig._

case class Example(defaultedString: String = "default", configuredString: String, optionalString: Option[String])

println(decode[Example]) // Invalid(NonEmptyList(ValidationFailure("Could not find configuration at 'example.configuredstring' and no default available", None)))

System.setProperty("example.configuredstring", "configured")
println(decode[Example]) // Valid(Example("default", "configured", None))

System.setProperty("example.optionalsting", "optional")
println(decode[Example]) // Valid(Example("default", "configured", Some("optional")))
```

## Nested Case Classes

```tut:silent
import extruder.system.SystemPropertiesConfig._

case class NestedTwo(value: String)
case class NestedOne(value: String, nested: NestedTwo)
case class Example(a: NestedOne, b: NestedTwo)

System.setProperty("example.a.nestedone.value", "nested-one")
System.setProperty("example.a.nestedone.nested.nestedtwo.value", "nested-one-nested-two")
System.setProperty("example.b.nestedtwo.value", "nested-two")

println(decode[Example]) // Valid(Example(NestedOne("nested-one", NestedTwo("nested-one-nested-two")), NestedTwo("nested-two"))
```
## Sealed Type Families

```tut:silent
import extruder.system.SystemPropertiesConfig._

sealed trait Sealed
case object ExampleObj extends Sealed
case class ExampleCC(a: Int) extends Sealed

System.setProperty("type", "ExampleObj")

println(decode[Sealed]) // Valid(ExampleObj)

System.setProperty("type", "ExampleCC")
System.setProperty("examplecc.a", "1")

println(decode[Sealed]) // Valid(ExampleCC(1))
```

```tut:silent
import extruder.system.SystemPropertiesConfig._

sealed trait Sealed
case object ExampleObj extends Sealed
case class ExampleCC(a: Int) extends Sealed

case class Example(a: Sealed)

System.setProperty("example.a.type", "ExampleObj")

println(decode[Example]) // Valid(Example(ExampleObj))

System.setProperty("example.a.type", "ExampleCC")
System.setProperty("example.a.examplecc.a", "1")

println(decode[Example]) // Valid(Example(ExampleCC(1)))

```

# Unsupported Functionality

**Cyclical references**
```scala
import extruder.system.SystemPropertiesConfig._

case class Example(e: Example)

decode[Example] // won't compile

case class NestedOne(n: NestedTwo)
case class NestedTwo(n: NestedOne)

decode[NestedOne] // won't compile
```
