---
layout: docs
title:  "Extruder Concepts"
position: 1
---
* TOC
{:toc}

# Overview

Extruder provides some default methods for decoding and encoding case classes from differing data sources which returns the value wrapped in monad capable of representing any possible errors. When decoding this may just be parsing failures, and when encoding there may be no errors so the monad instance may be `cats.Id` i.e. `type Id[A] = A`.

Extruder provides a newtype called `Validation` which is an `Either` where left hand side of errors is [cats] `NonEmptyList` instance. This is just a default implementation, extruder allows you to specify your own target monads.

## Data Source

A data source is an implementation of a number of traits which allow decoding or encoding types from a specified type of data, e.g. a Map or System Properties.

Functionality for decoding and encoding may be implemented separately, this comes in handy for when the underlying data is read-only ([see the environment data source](https://github.com/janstenpickle/extruder/blob/master/system-sources/src/main/scala/extruder/system/EnvironmentDataSource.scala)).

When implementing decoding or encoding functionality you will have to specify intermediate data types. This allows you to convert from and to the source/target data type. See [instructions for creating your own data source for more information](data_source.html)

## Decoder and Encoder

Decoders and Encoders are type classes which are able to extract or encode a specified type from or to a data source. Each implementation of a data source will have its own specific `EncoderT` and `DecoderT` traits specific to the data type.

## Effects

Depending on the [type class](type-classes.html) available for a data source, Extruder may require implementations of [Cats Effect](https://typelevel.org/cats-effect/) type class instances. For example, if a `DecoderT` instance reads from a key value store, side effects will have to be contained by the [`Async` type class](https://typelevel.org/cats-effect/typeclasses/async.html). Therefore compilation will fail when attempting to decode with a monad which cannot provide the [`Async` type class](https://typelevel.org/cats-effect/typeclasses/async.html). 

[Monad transformers are available](datatypes.html#evalvalidation) for separating the concers of side-effect and validation.

See the [decode/encode](decode_encode.html) documentation on how to use these typeclasses effectively and specifying a target monad.

{% include references.md %}
