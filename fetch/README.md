# Extruder Fetch
This module provides traits for using extruder with [Fetch](https://github.com/47deg/fetch). Two traits are available: [`FetchResolvers`](src/main/scala/extruder/fetch/FetchResolvers.scala), which uses a single data source to lookup strings and uses the primitive string parsing from the `Resolvers` trait in `core` and [`MultiDSFetchResolvers`](src/main/scala/extruder/fetch/MultiDSFetchResolvers.scala), which requires a [Fetch `DataSource`](http://47deg.github.io/fetch/docs.html#usage-2-writing-your-first-data-source-0) for each type to be resolved.
## `FetchResolvers`
As mentioned above, `FetchResolvers` requires only one data source, capable of looking up string values. It may be used directly or extended as you see fit.
### Using `FetchResolvers` Directly
To use `FetchResolvers` directly you must provide a [Fetch `DataSource`](http://47deg.github.io/fetch/docs.html#usage-2-writing-your-first-data-source-0), a function for converting from the config path (`Seq[String]`) to the data source's identity type and an optional timeout (the default timeout is one minute).

Here is an example usage of `FetchResolvers`:

```scala
import extruder.fetch.FetchResolvers
import fetch._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class Identity(table: String, key: String)

val ds: DataSource[Identity, String] = ??? // provide a fetch data source here
val table: String = "someTable"

val resolvers: FetchResolvers[Identity] = FetchResolvers(ds, path => Identity(table, path.mkString(".")), 30.seconds)
```

### Implementing `FetchResolvers`
When implementing `FetchResolvers` you must provide the same things as when using the apply method directly, with exception to the `pathToString` method, which is used for error messages. In the apply method this is implemented simply as `pathToIdentity(path).toString`.


Below shows a skeletal implementation:
```scala
class FetchResolversImpl[I] extends FetchResolvers[I] {
  override implicit def ec: ExecutionContext = ???
  override implicit def ds: DataSource[I, String] = ???
  override def timeout: Duration = ???
  override def pathToIdentity(path: Seq[String]): I = ???
  override def pathToString(path: Seq[String]): String = ???
}
```
## `MultiDSFetchResolvers`
When implementing `MultiDSFetchResolvers` you must provide a [Fetch `DataSource`](http://47deg.github.io/fetch/docs.html#usage-2-writing-your-first-data-source-0) for each type you wish to resolve, with exception to implementations of `TraverseableOnce`, where you will only need to provide a `DataSource[I, List[T]]` and all other resolvers will be derivied automatically.

Below is an example skeleton implementation:
```scala
class MultiDsFetchResolversImpl[I] extends MultiDsFetchResolvers[I] {
    override implicit def ec: ExecutionContext = ???
    override def timeout: Duration = ???
    override def pathToIdentity(path: Seq[String]): I = ???
    override def pathToString(path: Seq[String]): String = ???

    implicit def listDs: DataSource[I, List[String]] = ???
    implicit def stringDs: DataSource[I, String] = ???
    implicit def intDs: DataSource[I, Int] = ???
    implicit def longDs: DataSource[I, Long] = ???
    // add more `DataSource`s as required
  }
```
