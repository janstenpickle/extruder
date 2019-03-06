---
layout: docs
title:  "Type Classes"
position: 5
---
* TOC
{:toc}

# Overview

Extruder is made up of a number of type classes which compose to perform the task of encoding and decoding. Each data source may define new instances of some of these, or rely on already provided and derived instances.


# `Encoder`

**[Scaladoc](api/extruder/core/Encoder.html)**

Top level type class for encoding data. Implicit instances may be automatically derived by combining [`Show`](#show) or [`MultiShow`](#multishow) with [`StringWriter`](#stringwriter). 

# `Decoder`

**[Scaladoc](api/extruder/core/Decoder.html)**

Top level type class for decoding data. Implicit instances may be automatically derived by combining [`Parser`](#parser) or [`MultiParser`](#multiparser) with [`StringReader`](#stringreader)


# `Show`

**[Scaladoc](api/extruder/core/Show.html)**

Converts a value into a string. Reimplimentation of [Cats' show](https://typelevel.org/cats/typeclasses/show.html).

# `Parser`

**[Scaladoc](api/extruder/core/Parser.html)**

Converts a string into either a certain type or a string describing a parse failure. Default instances are provided for a number primitive types.

# `MultiShow`

**[Scaladoc](api/extruder/core/MultiShow.html)**

Converts a value into a map of strings (`Map[String, String]`). Can be used to convert a multi param object which cannot be generically derived. 

# `MultiParser`

**[Scaladoc](api/extruder/core/MultiParser.html)**

Converts a map of strings (`Map[String, String]`) into a value. Can be used to parse a multi param object which cannot be generically derived. 

# `Transform`

**[Scaladoc](api/extruder/core/Transform.html)**

When encoding or decoding it may be useful to convert to some intermediate type from which the data can be written/read. For example when working with system properties `Properties` can be converted into `Map[String, String]` when decoding and written into `Map[String, String]` when encoding. 

`Transform` is very similar to a [Kleisli composition](https://typelevel.org/cats/datatypes/kleisli.html) in that it takes some input data, executes some function over it and returns the result inside of a functor.

A default no operation instance of transform is provided, which just passes through the data untouched.

# `StringReader`

**[Scaladoc](api/extruder/core/StringReader.html)**

Reads a string from a data source and returns it as an option.

# `StringWriter`

**[Scaladoc](api/extruder/core/StringWriter.html)**

Writes a string to a data source.

# `HasValue`

**[Scaladoc](api/extruder/core/HasValue.html)**

Discovers if a data source has a certain value. If an implicit [`StringReader`](#stringreader) instance is in scope, then a `HasValue` instance may be automatically derived.


# `Prune`

**[Scaladoc](api/extruder/core/Prune.html)**

Used to constrain input data under a given path. Can be used to decode a map from a data source. 

# `LoadInput`

**[Scaladoc](api/extruder/core/LoadInput.html)**

Certain data soruces may be loaded from the environment such as environment variables and system properties. `LoadInput` provides the ability to load this data for these data sources.


# `ExtruderErrors`

**[Scaladoc](api/extruder/core/ExtruderErrors.html)**

Church encoding of types of errors which may be encountered during the encoding or decoding process. Also provides the ability to fallback to another invocation.

Implicit instances may be automatically derived for instances of [Cats' `ApplicativeError[F, Throwable]`](https://typelevel.org/cats/api/cats/ApplicativeError.html).

# `ValidationErrorsToThrowable`

**[Scaladoc](api/extruder/core/ValidationErrorsToThrowable.html)**

Flattens validation errors into a single exception. Used in the `cats-effect` module by [`EffectValidations`](datatypes.html#effectvalidations) to provide `ConcurrentEffect` instances.