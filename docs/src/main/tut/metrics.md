---
layout: docs
title:  "Metrics"
position: 7
---
* TOC
{:toc}

# Overview
The extruder metrics modules provide support for encoding case classes as name-spaced or dimensional metrics.

# Metric Encoding

Exturder supports two types of metric encoding schemes, name-spaced and dimensional. Both are described below:

## Name-spaced Metrics
Name-spaced metrics are most commonly associated with [Graphite](https://graphiteapp.org/), where they are simple key values arranged in a hierarchy of common dot-seprated key elements.

Metrics are encoded using the path to the value in the case class structure e.g.

```scala
case class Test1(value: Int = 1000)
case class Test2(metrics: Test1)
```
Will be encoded as `test2.metrics.test1.value = 1000`.

## Dimensional metrics
Dimensional metrics are most commonly associated with [Prometheus](https://prometheus.io/) where a metric has multiple labels which then may be queried in aggregate with other metrics which share the same dimensions.

Metrics in a case class can be encoded as follows, providing there is enough depth in the case class structure to be able to create dimensions:

```scala
case class StatusCode(`200`: Int = 2043, `500`: Int = 3, other: Int = 653)
case class RequestCount(httpRequests: StatusCode)
```

Will be encoded as:
```
{name=http_requests,status_code=200} = 2043
{name=http_requests,status_code=500} = 3
{name=http_requests,status_code=other} = 653
```
This allows you to aggregate all `http_requests` values by not including the `status_code` dimension in a query.


Alternatively if there is not enough depth in the case class structure then names will be encoded as the parameter names of the case class:

```scala
case class HttpResponses(`200Responses`: Int = 2043, `500Responses`: Int = 3, otherResponses: Int = 653)
```

Will be encoded as:
```
{name=200_responses,status_code=200} = 2043
{name=500_responses,status_code=500} = 3
{name=other_responses,status_code=other} = 653
```

If you don't know the possible label values ahead of time it is possible to encode a map of string to number which represents each dimension value and its corresponding reading:

```scala
case class HttpRequests(statusCode: Map[String, Int] = Map("200" -> 2043, "500" -> 3, "401" -> 7643, "other" -> 12))
```

Will be encoded as:

```
{name=http_requests,status_code=200} = 2043
{name=http_requests,status_code=500} = 3
{name=http_requests,status_code=400} = 7643
{name=http_requests,status_code=other} = 12
```

## Lack of depth

If your case class heirarchy is not deep enough, then extruder will encode the case class's field names as metrics. For example:

```scala
case class StatusCode(`200`: Int = 2043, `500`: Int = 3, other: Int = 653)
```

Will be encoded as:
```
{name=200} = 2043
{name=500} = 3
{name=other} = 653
```

## Additional dimensions

When instantiating an implementation of a dimensional metric encoder you may provide optional additional dimensions as a string map (`Map[String, String]`). This allows you to specify extra metadata such as hostname or job instance to every metric recorded.

### Encoding and compilation

Note that dimensional metric encoders will fail to compile case classes which cannot be properly represented as dimensional metrics, i.e. any case class whose fields would be encoded as separate labels would not compile if those fields are not all the same type.

# Metric Types

In order to encode a type of metric extruder includes a trait called `MetricValue`. Provided implementations include `Counter`, `Gauge` and `Timer`, each being a [value class](https://docs.scala-lang.org/overviews/core/value-classes.html). A case class which uses these allows for fine grained control over the type of metric for each value.

To encode multiple dimensions in a `Map`, as covered above, the type `MetricValues` is provided, which is a [value class](https://docs.scala-lang.org/overviews/core/value-classes.html) class wrapper around `Map[String, MetricValue[V]]` where `V` has an implicit `Numeric` instance. An implicit monoid instance is also providied for this type.

For example it is possible to 

```tut:silent
import extruder.metrics.data.CounterValue
import extruder.metrics.conversions.counter._ //implicitly converts numbers to CounterValue

case class StatusCode(`200`: CounterValue[Int] = 2043, `500`: CounterValue[Int] = 3, other: CounterValue[Int] = 653)
case class RequestCount(httpRequests: StatusCode)
```

If you don't wish to use `MetricValue` or `MetricValues` in your case classes, encoding will still work (as seen above), however the values will be encoded as the default metric type for the encoder.

# Label Values

Any type which cannot be encoded as a numeric value can be encoded as a label where the value of the field will be included in the name of the metric and the metric value set to `1`. For example, consider the following case class:

```scala
case class Stats(status: String = "OK")
```

Will be encoded as below in name-spaced metrics:
```
stats.status.OK = 1
```
Or in dimensional metrics:
```
{status=OK} = 1
```
# Provided Encoders

Extruder currently provides three types of encoders, although anyone is welcome to add more:

## Dropwizard

The [Dropwizard](http://metrics.dropwizard.io) package provides both name-spaced and dimensional metric encoders which records values in a Dropwizard metrics registry.

```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "io.extruder" %% "extruder-metrics-dropwizard" % "0.10.0"
```

## Prometheus

The [Prometheus](https://prometheus.io/) package provides two dimensional metric encoders, one which records values in a Prometheus metrics registry and one which sends metrics to a [Push Gateway](https://github.com/prometheus/pushgateway).

```scala
resolvers += Resolver.bintrayRepo("janstenpickle", "maven")
libraryDependencies += "io.extruder" %% "extruder-metrics-prometheus" % "0.10.0"
```

## Spectator

The [Spectator](https://github.com/Netflix/spectator) package provides a dimensional metric encode which records in a Spectator metrics registry.

```scala
libraryDependencies += "io.extruder" %% "extruder-metrics-spectator" % "0.10.0"
```