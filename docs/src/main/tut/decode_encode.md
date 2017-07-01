---
layout: docs
title:  "Decoding and Encoding"
position: 3
---
* TOC
{:toc}

# Decoding and Encoding Basics
There are a few types of methods for encoding and decoding documented below, please see the API documentation for [decoding](api/extruder/core/Decode.html) and [encoding](api/extruder/core/Encode.html) for more details.

|---+---+---|
| Method Type|Description|Names|
|---+---+---|
|Simple|Decode or encode a certain type, wrapping the result in [`Validation`](api/extruder/core/index.html#Validation[T]=cats.data.ValidatedNel[extruder.core.ValidationError,T]) or a specified type with a corresponding implicit [`ExtruderApplicativeError`](api/extruder/core/ExtruderApplicativeError.html)|[`decode`](api/extruder/core/Decode.html) [`encode`](api/extruder/core/Encode.html)|
|Unsafe|Similar to simple except that any errors will be thrown as exceptions|[`decodeUnsafe`](api/extruder/core/Decode.html) [`encodeUnsafe`](api/extruder/core/Encode.html)|
|IO|The resulting output will be the same as _Simple_, except that the return type will also be wrapped in a [cats effect] `IO` monad|[`decodeIO`](api/extruder/core/Decode.html) [`encodeIO`](api/extruder/core/Encode.html)|
|Async|The resulting output will be the same as _Simple_, except that the return type will also be wrapped in a specified type, for which there should be an [`IOConvert`](api/extruder/core/IOConvert.html) instance|[`decodeAsync`](api/extruder/core/Decode.html) [`encodeAsync`](api/extruder/core/Encode.html)|

Normally when decoding you are required to pass the data and an optional namespace, however some data sources may provide default data, such as from [system properties](api/extruder/system/SystemPropertiesSource.html), [environment variables](api/extruder/system/EnvironmentConfig$.html) or [typesafe config](api/extruder/typesafe/TypesafeConfigSource$.html). These all extend the trait [`DecodeFromDefaultSource`](api/extruder/core/DecodeFromDefaultSource.html) which includes a set of `decode` methods which do not require data to be passed.

## Advanced Examples
Extruder includes some default implementations for `IO`, `Future`, `Either` and `ValidatedNel`, as well as extension modules for [Monix] and [FS2].

```tut:silent
import cats.effect.IO
import extruder.core._
import extruder.core.EitherThrowable
import extruder.core.MapSource._
import extruder.monix._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Future

decode[String, IO, Throwable](List("a"), Map("a" -> "b")) // returns IO("b")
decode[String, Future, Throwable](List("a"), Map("a" -> "b")) // returns Future("b")
decode[String, Task, Throwable](List("a"), Map("a" -> "b")) // returns Task("b")

decodeIO[String, EitherThrowable, Throwable](List("a"), Map("a" -> "b")) // returns IO(Right("b"))

decodeAsync[String, Task](List("a"), Map("a" -> "b")) // returns Task(Valid("b"))
decodeAsync[String, Task, EitherThrowable, Throwable](List("a"), Map("a" -> "b")) // returns Task(Right("b"))
```

A couple of things to note in the examples above:
- `monix.execution.Scheduler` is an extension of `scala.concurrent.ExecutionContext` so there is no need to import `scala.concurrent.ExecutionContext.Implicits.global`, however whenever using `IO` or `Future` you should have an `ExecutionContext` in implicit scope.
- `EitherThrowable` is a type alias of `Either[Throwable, T]` in `extruder.core`, it would be normal to use [kind projector] here, however this can result in compile failures.

# Implementing Your Own Target Monads

As mentioned in [Concepts](concepts.html), Extruder allows you to specify target monads for your desired return types. Internally the side effects are modelled with the [cats effect] `IO` monad and validation failures are handled by a user specified monad. Values retrieved from asynchronous data sources may be lifted into the `IO` monad using an `IOConvert` instance.

## `Try` Example

The example below demonstrates what is needed to create implicit instances to allow the response to be wrapped in a `Try`:

```tut:silent
import extruder.core._
import extruder.core.MapSource._
import cats.instances.TryInstances
import cats.effect.IO
import scala.util.{Failure, Success, Try}

object TryExtruderInstances extends TryInstances {
  implicit val tryAE: ExtruderApplicativeError[Try, Throwable] =
    new ExtruderApplicativeError.FromMonadError[Try]

  implicit val tryIOFlatmap: IOFlatMap[Try] =
    new IOFlatMap[Try] {
      override def flatMap[A, B](fa: IO[Try[A]])(f: (A) => IO[Try[B]]): IO[Try[B]] =
        fa.flatMap {
          case Success(v) => f(v)
          case Failure(e) => IO(Failure(e))
        }    
    }
}
```
You can then import your new instances object to decode and encode using `Try` to encode success and error paths:

```tut:silent
import TryExtruderInstances._

encode[String, Try, Throwable](List("a"), "b")
decode[String, Try, Throwable](List("a"), Map("a" -> "b"))
```

## `Either` Example

Although Extruder core implements `Either` instances for both `Either[Throwable, T]` and `Either[NonEmptyList[ValidationError], T]`, the example below shows a clean example for `Either[NonEmptyList[ValidationError], T]`.

It also demonstrates how when overriding the `ap` method it is possible to accumulate errors on the left hand side:  

```tut:silent
import extruder.core._
import extruder.core.MapSource._
import cats.instances.EitherInstances
import cats.syntax.either._
import cats.effect.IO

type EitherErrors[T] = Either[ValidationErrors, T]

object EitherExtruderInstances extends EitherInstances {
  implicit val eitherAccAE: ExtruderApplicativeError[EitherErrors, ValidationErrors] =   
    new ExtruderApplicativeError.FromMonadErrorAccumulatesErrors[EitherErrors] {
      override def ap[A, B](ff: EitherErrors[(A) => B])(fa: EitherErrors[A]): EitherErrors[B] = (ff, fa) match {
        case (Right(f), Right(a)) => Right(f(a))
        case (Left(fe), Left(ae)) => Left(fe ++ ae.toList)
        case (Left(e), Right(_)) => Left(e)
        case (Right(_), Left(e)) => Left(e)
      }
    }

  implicit val eitherIOFlatMap: IOFlatMap[EitherErrors] =
    new IOFlatMap[EitherErrors] {
      override def flatMap[A, B](fa: IO[EitherErrors[A]])
                                (f: (A) => IO[EitherErrors[B]]): IO[EitherErrors[B]] =
        fa.flatMap {
          case Right(v) => f(v)
          case Left(e) => IO(Left(e))
        }
    }
}
```

As with `Try` it is now possible to import your instances to decode and encode using `Either` to encode success and error paths:

```tut:silent
import EitherExtruderInstances._

encode[String, EitherErrors, ValidationErrors](List("a"), "b")
decode[String, EitherErrors, ValidationErrors](List("a"), Map("a" -> "b"))
```
{% include references.md %}
