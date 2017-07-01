---
layout: docs
title:  "Extruder Concepts"
position: 1
---
* TOC
{:toc}

# Overview

Extruder provides some default methods for decoding and encoding case classes from differing data sources which returns the value wrapped in a [cats] `ValidatedNel` instance. However this is just the default implementation, extruder allows you to specify your own target monads.

The design of extruder assumes that reading and writing data could be side affecting and therefore requires that whenever a value is read from data it is wrapped in the [cats effect] `IO` monad, as well as the extruder typeclass for aggregating errors; `ExtruderApplicativeError`.

1. `IO` monad models side effects in loading data and fails on the first error.
1. `ExtruderApplicativeError` models errors in decoding data values and optionally aggregates those errors depending on the target monad.

For more information on how to use different monads with extruder see the [documentation on decoding and encoding](decode_encode.html)

## Why use the `IO` monad and not `Effect`?

Extruder acts like a bridge between data and a target type. The target type may be wrapped an an effect monad and a validation monad of the user's choosing (1 and 2 above), provided the required [implicits exist](decode_encode.html). In certain sections of Extruder internals, both the side affect and and validation monad must be flat-mapped together. This causes a problem when both are unknown because `flatMap` instances for all permutations of effect and validation monads must be provided. Instead the reference implementation of [cats effect], `IO`, is used as a concrete target for flat-mapping and side-effecting monads, such as `Future` or [Monix]'s `Task` may be converted to and from `IO`.

An example of how this may work from data source to result is shown below
<div class="mermaid">
graph LR;
    S[Data Source - Future]-->E[Extruder - IO];
    E[Extruder IO]-->R[Result - Task];
</div>
# Terms

Some of the key terms for extruder are described below:

## Data Source

A data source is an implementation of a number of traits which allow decoding or encoding types from a specified type of data, e.g. a Map or System Properties.

Functionality for decoding and encoding may be implemented separately, this comes in handy for when the underlying data is read-only ([see the environment data source](https://github.com/janstenpickle/extruder/blob/master/system-sources/src/main/scala/extruder/system/EnvironmentSource.scala)).

When implementing decoding or encoding functionality you will have to specify intermediate data types. This allows you to convert from and to the source/target data type. See [instructions for creating your own data source for more information](data_source.html)

## Decoder and Encoder

Decoders and Encoders are type classes which are able to extract or encode a specified type from or to a data source. Each implementation of a data source will have its own specific `Encoder` and `Decoder` traits specific to the data type.

{% include references.md %}
