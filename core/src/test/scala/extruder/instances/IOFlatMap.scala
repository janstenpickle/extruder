package extruder.instances

import cats.effect.IO
import cats.instances.all._
import cats.{Eq, Eval}
import extruder.core.TestCommon._
import extruder.core.{ConfigValidation, ConfigValidationCatsInstances, IOFlatMapSpec, ValidationErrors}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

class FutureIOFlatMapSpec extends IOFlatMapSpec[Future, Throwable] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Future[A]] = futureEq

  override implicit def IOFEq[A](implicit e: Eq[Future[A]], ea: Eq[A]): Eq[IO[Future[A]]] = new Eq[IO[Future[A]]] {
    override def eqv(a: IO[Future[A]], b: IO[Future[A]]): Boolean = {
      val a1 = Try(IO.fromFuture(Eval.later(a.unsafeRunSync())).unsafeRunSync())
      val b1 = Try(IO.fromFuture(Eval.later(b.unsafeRunSync())).unsafeRunSync())

      tryEq[A].eqv(a1, b1)
    }
  }
}
class EitherIOFlatMapSpec extends IOFlatMapSpec[Either[Throwable, ?], Throwable] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Either[Throwable, A]] = Eq[String].on(_.toString)
}
class EitherNelIOFlatMapSpec extends IOFlatMapSpec[Either[ValidationErrors, ?], ValidationErrors] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Either[ValidationErrors, A]] = Eq.fromUniversalEquals
}
class ConfigValidationIOFlatMapSpec extends IOFlatMapSpec[ConfigValidation, ValidationErrors] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[ConfigValidation[A]] = ConfigValidationCatsInstances.validationErrorsEq
}
class IOIOFlatMapSpec extends IOFlatMapSpec[IO, Throwable] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[IO[A]] = ioEq

  override implicit def IOFEq[A](implicit e: Eq[IO[A]], ea: Eq[A]): Eq[IO[IO[A]]] = new Eq[IO[IO[A]]] {
    override def eqv(a: IO[IO[A]], b: IO[IO[A]]): Boolean = {
      val a1 = Try(a.unsafeRunSync().unsafeRunSync())
      val b1 = Try(b.unsafeRunSync().unsafeRunSync())

      tryEq[A].eqv(a1, b1)
    }
  }
}
