---
layout: docs
title:  "Extruder Concepts"
position: 1
---
* TOC
{:toc}

# Overview

Extruder provides some default methods for decoding and encoding case classes from differing data sources which returns the value wrapped in a `Either` where left hand side of errors is  [cats] `NonEmptyList` instance. However this is just the default implementation, extruder allows you to specify your own target monads.

## Effects

Extruder use [cats][cats] and [cats effect][cats effect] to define target monads. By default, when encoding or decoding, Extruder returns an `Either[ValidatedNel[ValidationError], A]` where, errors are collected on the left hand side.

Extruder extends the `MonadError`, `Sync` and `Async` typeclasses from the Typelevel cats ecosystem. Each of these typeclasses handle effects in different ways, it us up to the maintainer of the [data source](data_source.html) as to which typeclass models the side effects of their target backend. For example the provided data source for `Map[String, String]` uses `MonadError` because there are no side effects in looking up from an already initialised map. However a data source implementation for reading from a database should use the `Async` typeclass so that any asynchronous computation is captured and handled correctly.

Extruder provides types for separating validation errors from effects via the `EitherT` monad transformer, combined with one of the typeclasses mentioed above. For example `EitherT[IO, ValidatedNel[ValidationError], A]`'s underlying value is `IO[Either[ValidatedNel[ValidationError], A]]` where side effects of asynchronous computation are captured by the [cats effect][cats effect] `IO` monad and validation errors are modelled by `Either[ValidatedNel[ValidationError], A]`. The same is true for any implementation of `Sync` or `Async` typeclasses such the [Monix][monix] `Task`.

There is a hierarchy of power when it comes to these typeclasses, where `Async` > `Sync` > `MonadError`. This means that an `Async` typeclass implementation may be used for data sources specifying `MonadError` as their target monad, however the reverse isn't true.

> Note that the default `decode` method uses a `MonadError` instance, so if the backend you are using requires an `Async` or `Sync` target you must specify the type manually.

See the [examples](examples.html) documentation on how to use these typeclasses effectively and specifying a target monad.

# Terms

Some of the key terms for extruder are described below:

## Data Source

A data source is an implementation of a number of traits which allow decoding or encoding types from a specified type of data, e.g. a Map or System Properties.

Functionality for decoding and encoding may be implemented separately, this comes in handy for when the underlying data is read-only ([see the environment data source](https://github.com/janstenpickle/extruder/blob/master/system-sources/src/main/scala/extruder/system/EnvironmentSource.scala)).

When implementing decoding or encoding functionality you will have to specify intermediate data types. This allows you to convert from and to the source/target data type. See [instructions for creating your own data source for more information](data_source.html)

## Decoder and Encoder

Decoders and Encoders are type classes which are able to extract or encode a specified type from or to a data source. Each implementation of a data source will have its own specific `Encoder` and `Decoder` traits specific to the data type.

{% include references.md %}
