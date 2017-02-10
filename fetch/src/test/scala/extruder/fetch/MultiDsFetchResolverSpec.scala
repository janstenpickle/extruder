package extruder.fetch
import cats.Eval
import cats.data.NonEmptyList
import extruder.core.{Extruder, ValidationFailure}
import fetch._
import org.specs2.Specification
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

class MultiDsFetchResolverSpec extends Specification with EitherMatchers {
  import MultiDsFetchResolverSpec._

  override def is: SpecStructure = s2"""
    Can successfully resolve a case class from
      synchronous data sources $syncSuccess
      asynchronous data sources $asyncSuccess
      synchronous data sources using defaults $missingSync
      asynchronous data sources using defaults $missingAsync
      optional data values $opt
      defaulted optional data values $defaultOpt
    Fails to resolve a case class when
      the asynchronous operation fails $failingAsync
      values for a case class with no defaults $missing
  """

  def syncSuccess: Result[Defaults] = {
    import SyncRes._
    Extruder[Defaults](SyncRes).productResolver.read(Seq.empty, None).toEither must beRight(valid)
  }

  def asyncSuccess: Result[Defaults] = {
    import AsyncRes._
    Extruder[Defaults](SyncRes).productResolver.read(Seq.empty, None).toEither must beRight(valid)
  }

  def missingSync: Result[Defaults] = {
    import MissingSyncRes._
    Extruder[Defaults](MissingSyncRes).productResolver.read(Seq.empty, None).toEither must beRight(default)
  }

  def missingAsync: Result[Defaults] = {
    import MissingAsyncRes._
    Extruder[Defaults](MissingAsyncRes).productResolver.read(Seq.empty, None).toEither must beRight(default)
  }

  def opt: Result[Defaults] = {
    import OptionalRes._
    Extruder[Defaults](OptionalRes).productResolver.read(Seq.empty, None).toEither must beRight(
      Defaults(valid.a, valid.b, Some(2L))
    )
  }

  def defaultOpt: Result[OptionalDefaults] = {
    import MissingSyncRes._
    Extruder[OptionalDefaults](MissingSyncRes).productResolver.read(Seq.empty, None).toEither must beRight(OptionalDefaults())
  }

  def failingAsync: Result[Defaults] = {
    import FailingAsyncRes._

    def expectedError(param: String) = new ValidationFailure(
      s"Failed to resolve value for 'defaults.$param': '$error'", Some(Ex)
    )

    Extruder[Defaults](FailingAsyncRes).productResolver.read(Seq.empty, None).toEither must beLeft.which(err =>
      err.toList.size === 4 and
      (err.toList must containTheSameElementsAs(
        List(expectedError("a"), expectedError("b"), expectedError("c"), expectedError("d"))
      ))
    )
  }

  def missing: Result[NoDefaults] = {
    import MissingSyncRes._

    def expectedError(param: String) = new ValidationFailure(
      s"Could not find configuration at 'nodefaults.$param' and no default available", None
    )

    Extruder[NoDefaults](MissingSyncRes).productResolver.read(Seq.empty, None).toEither must beLeft.which(err =>
      err.toList.size === 2 and (err.toList must containTheSameElementsAs(List(expectedError("a"), expectedError("b"))))
    )
  }
}

object MultiDsFetchResolverSpec {
  type Result[T] = MatchResult[Either[NonEmptyList[ValidationFailure], T]]

  case class Defaults(a: String = "default", b: Int = 1, c: Option[Long], d: Set[String] = Set.empty)
  val default = Defaults("default", 1, None)
  val valid: Defaults = Defaults("test", 2, None)

  case class NoDefaults(a: String, b: Seq[String])
  case class OptionalDefaults(a: Option[String] = Some("default"))

  val error = "error"

  def ds[T](value: T): DataSource[String, T] = dsQuery[T](Query.eval(Eval.now(Some(value))))

  def dsQuery[T](value: Query[Option[T]]): DataSource[String, T] = new DataSource[String, T] {
    override def fetchOne(id: String): Query[Option[T]] = value

    override def fetchMany(ids: NonEmptyList[String]): Query[Map[String, T]] = Query.eval(Eval.now(
      Map.empty
    ))
  }

  trait Res extends MultiDsFetchResolvers[String] {
    override implicit val ec: ExecutionContext = ExecutionContext.global

    override val timeout: Duration = Duration.Inf

    override def pathToIdentity(path: Seq[String]): String = pathToString(path)

    override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase

    def ds[T](value: T): DataSource[String, T]
    def ds[T]: DataSource[String, T]

    implicit val listDs: DataSource[String, List[String]] = ds(valid.d.toList)
    implicit val stringDs: DataSource[String, String] = ds(valid.a)
    implicit val intDs: DataSource[String, Int] = ds(valid.b)
    implicit val longDs: DataSource[String, Long] = ds
  }

  object SyncRes extends Res {
    override def ds[T](value: T): DataSource[String, T] = dsQuery(Query.eval(Eval.now(Some(value))))
    override def ds[T]: DataSource[String, T] = dsQuery(Query.eval(Eval.now(None)))
  }

  object AsyncRes extends Res {
    override def ds[T](value: T): DataSource[String, T] = dsQuery(Query.async((ok, _) => ok(Some(value))))
    override def ds[T]: DataSource[String, T] = dsQuery(Query.async((ok, _) => ok(None)))
  }

  object MissingSyncRes extends Res {
    override def ds[T](value: T): DataSource[String, T] = ds
    override def ds[T]: DataSource[String, T] = dsQuery(Query.eval(Eval.now(None)))
  }

  object MissingAsyncRes extends Res {
    override def ds[T](value: T): DataSource[String, T] = dsQuery(Query.async((ok, _) => ok(None)))
    override def ds[T]: DataSource[String, T] = dsQuery(Query.async((ok, _) => ok(None)))
  }

  object OptionalRes extends Res {
    override def ds[T](value: T): DataSource[String, T] = dsQuery(Query.eval(Eval.now(Some(value))))
    override def ds[T]: DataSource[String, T] = dsQuery(Query.eval(Eval.now(None)))

    override implicit val longDs: DataSource[String, Long] = ds(2L)
  }

  object FailingAsyncRes extends Res {
    override def ds[T](value: T): DataSource[String, T] = dsQuery(Query.async((_, fail) => fail(Ex)))
    override def ds[T]: DataSource[String, T] = dsQuery(Query.async((_, fail) => fail(Ex)))
  }

  object Ex extends Exception(error) {
    override def equals(obj: scala.Any): Boolean = obj match {
      case ex: Ex.type => getMessage == ex.getMessage
      case _ => false
    }
  }
}

