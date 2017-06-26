package extruder.instances

import cats.Eq
import cats.effect.IO
import cats.instances.all._
import extruder.core._
import extruder.core.TestCommon._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class FutureExtruderApplicativeError extends ExtruderApplicativeErrorThrowableSpec[Future]() {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Future[A]] = futureEq
}

class ConfigValidationExtruderApplicativeError extends ExtruderApplicativeErrorValidationErrorsSpec[ConfigValidation]

class EitherThrowableExtruderApplicativeError extends ExtruderApplicativeErrorThrowableSpec[Either[Throwable, ?]] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[Either[Throwable, A]] = Eq[String].on(_.toString)
}

class EitherValidationErrorsExtruderApplicativeError extends ExtruderApplicativeErrorValidationErrorsSpec[Either[ValidationErrors, ?]]

class IOExtruderApplicativeError extends ExtruderApplicativeErrorThrowableSpec[IO] {
  override implicit def feq[A](implicit e: Eq[A]): Eq[IO[A]] = ioEq
}
