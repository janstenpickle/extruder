package extruder.instances

//import cats.data.Validated.{Invalid, Valid}
//import cats.data.{NonEmptyList, Validated}
//import cats.syntax.validated._
//import cats.{Apply, Eval, Monad}
//import extruder.core
//import extruder.core.{Missing, Validation, ValidationError, ValidationException, ValidationFailure}
//import extruder.data.ValidationT
//import extruder.effect.{ExtruderMonadError, ExtruderSync}
//
//import scala.annotation.tailrec
//import scala.util.control.NonFatal

trait ValidationInstances {
//  implicit val validationMonadError: ExtruderMonadError[Validation] =
//    new ExtruderMonadError[Validation] {
//
//      @tailrec
//      def tailRecM[A, B](a: A)(f: A => Validation[Either[A, B]]): Validation[B] =
//        f(a) match {
//          case err @ Invalid(_) =>
//            err
//          case Valid(e) =>
//            e match {
//              case Left(a1) => tailRecM(a1)(f)
//              case Right(b) => Valid(b)
//            }
//        }
//
//      override def flatMap[A, B](fa: Validation[A])(f: (A) => Validation[B]): Validation[B] = fa.fold(_.invalid, f)
//
//      override def raiseError[A](e: Throwable): Validation[A] = ValidationException(e.getMessage, e).invalidNel
//
//      override def ap[A, B](ff: Validation[(A) => B])(fa: Validation[A]): Validated[NonEmptyList[ValidationError], B] =
//        fa.ap(ff)
//
//      override def handleErrorWith[A](fa: Validation[A])(f: (Throwable) => Validation[A]): Validation[A] =
//        fa.fold(e => f(core.errorsToThrowable(e)), Valid(_))
//
//      override def product[A, B](fa: Validation[A], fb: Validation[B]): Validation[(A, B)] = fa.product(fb)
//
//      override def missing[A](message: String): Validation[A] = Missing(message).invalidNel
//
//      override def validationFailure[A](message: String): Validation[A] =
//        ValidationFailure(message).invalidNel
//
//      override def validationException[A](message: String, ex: Throwable): Validation[A] =
//        ValidationException(message, ex).invalidNel
//
//      override def pure[A](x: A): Validation[A] = x.validNel
//    }
//
//  implicit val validationTEvalSync: ExtruderSync[ValidationT[Eval, ?]] = new ExtruderSync[ValidationT[Eval, ?]] {
//    override def suspend[A](thunk: => ValidationT[Eval, A]): ValidationT[Eval, A] =
//      ValidationT[Eval, A](Eval.always(try {
//        thunk.value.value
//      } catch {
//        case NonFatal(t) => ValidationException(t.getMessage, t).invalidNel
//      }))
//
//    override def validationException[A](message: String, ex: Throwable): ValidationT[Eval, A] =
//      ValidationT[Eval, A](Eval.now(ValidationException(message, ex).invalidNel))
//
//    override def missing[A](message: String): ValidationT[Eval, A] =
//      ValidationT[Eval, A](Eval.now(Missing(message).invalidNel))
//
//    override def validationFailure[A](message: String): ValidationT[Eval, A] =
//      ValidationT[Eval, A](Eval.now(ValidationFailure(message).invalidNel))
//
//    override def raiseError[A](e: Throwable): ValidationT[Eval, A] = validationException(e.getMessage, e)
//
//    override def handleErrorWith[A](
//      fa: ValidationT[Eval, A]
//    )(f: Throwable => ValidationT[Eval, A]): ValidationT[Eval, A] =
//      ValidationT(
//        fa.value.flatMap(_.fold(errs => f.andThen(_.value)(core.errorsToThrowable(errs)), a => Eval.now(Valid(a))))
//      )
//
//    override def pure[A](x: A): ValidationT[Eval, A] = ValidationT.liftF(Eval.now(x))
//
//    override def tailRecM[A, B](a: A)(f: A => ValidationT[Eval, Either[A, B]]): ValidationT[Eval, B] = {
//      val F: Monad[Eval] = Monad[Eval]
//
//      ValidationT[Eval, B](
//        F.tailRecM(a)(
//          a0 =>
//            f(a0).value.map {
//              case Invalid(e) => Right(Invalid(e))
//              case Valid(Left(a1)) => Left(a1)
//              case Valid(Right(b)) => Right(Valid(b))
//          }
//        )
//      )
//    }
//
//    override def flatMap[A, B](fa: ValidationT[Eval, A])(f: A => ValidationT[Eval, B]): ValidationT[Eval, B] =
//      ValidationT(fa.value.flatMap {
//        case err @ Invalid(_) => Eval.now(err)
//        case Valid(a) => f(a).value
//      })
//
//    override def ap[A, B](ff: ValidationT[Eval, A => B])(fa: ValidationT[Eval, A]): ValidationT[Eval, B] =
//      ValidationT(for {
//        fa0 <- fa.value
//        ff0 <- ff.value
//      } yield Apply[Validation].ap(ff0)(fa0))
//
//    override def ap2[A, B, Z](
//      ff: ValidationT[Eval, (A, B) => Z]
//    )(fa: ValidationT[Eval, A], fb: ValidationT[Eval, B]): ValidationT[Eval, Z] =
//      ValidationT(for {
//        fa0 <- fa.value
//        fb0 <- fb.value
//        ff0 <- ff.value
//      } yield Apply[Validation].ap2(ff0)(fa0, fb0))
//  }
}
