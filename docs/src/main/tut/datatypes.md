---
layout: docs
title:  "Data Types"
position: 6
---
* TOC
{:toc}

# Overview

# `ValidationError`

An algebraic data type representing the different types of failure Extruder may encounter duing encoding and decoding.

## `Missing`

Used when a required value cannot be found in a data source.

## `ValidationFailure`

Used when decoding fails.

## `ValidationException`

Used when validation fails with an exception.

# `Validation`

[Newtype](https://github.com/estatico/scala-newtype) of `Either[NonEmptyList[ValidationError], ?]` used in some of the default data source implementations, such as map, system properties and environment variables. 

# `EvalValidation`

[Newtype](https://github.com/estatico/scala-newtype) of `EitherT[Eval, NonEmptyList[ValidationError], ?]` in the cats effect module, which provides a [`Sync`](https://typelevel.org/cats-effect/typeclasses/sync.html) instance

# `EffectValidation`

[Newtype](https://github.com/estatico/scala-newtype) of `EitherT[F, NonEmptyList[ValidationError], ?]` in the cats effect module, which provides [cats effect typeclasses](https://typelevel.org/cats-effect/typeclasses/) for any `F` which has assocated instances.