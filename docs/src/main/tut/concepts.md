---
layout: docs
title:  "Extruder Concepts"
position: 1
---
* TOC
{:toc}

# Overview

Extruder provides some default methods for decoding and encoding case classes from differing data sources which returns the value wrapped in a `Either` where left hand side of errors is  [cats] `NonEmptyList` instance. However this is just the default implementation, extruder allows you to specify your own target monads.

## Effects?

Extruder use [cats][cats] and [cats effect][cats effect] to define target monads.

# Terms

Some of the key terms for extruder are described below:

## Data Source

A data source is an implementation of a number of traits which allow decoding or encoding types from a specified type of data, e.g. a Map or System Properties.

Functionality for decoding and encoding may be implemented separately, this comes in handy for when the underlying data is read-only ([see the environment data source](https://github.com/janstenpickle/extruder/blob/master/system-sources/src/main/scala/extruder/system/EnvironmentSource.scala)).

When implementing decoding or encoding functionality you will have to specify intermediate data types. This allows you to convert from and to the source/target data type. See [instructions for creating your own data source for more information](data_source.html)

## Decoder and Encoder

Decoders and Encoders are type classes which are able to extract or encode a specified type from or to a data source. Each implementation of a data source will have its own specific `Encoder` and `Decoder` traits specific to the data type.

{% include references.md %}
