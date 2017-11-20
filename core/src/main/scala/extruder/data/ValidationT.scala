package extruder.data

import cats.data.{EitherT, EitherTBifoldable, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import cats.{Applicative, Bifoldable, Bitraverse, Eq, Eval, Foldable, Functor, Monad, Order, PartialOrder, Traverse}
import extruder.core.{Validation, ValidationErrors}

final case class ValidationT[F[_], A](value: F[Validation[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): ValidationT[F, B] = ValidationT(F.map(value)(_.map(f)))

  def flatMap[B](f: A => ValidationT[F, B])(implicit F: Monad[F]): ValidationT[F, B] =
    ValidationT(F.flatMap(value) {
      case err @ Invalid(_) => F.pure(err)
      case Valid(b) => f(b).value
    })

  def flatMapF[B](f: A => F[Validation[B]])(implicit F: Monad[F]): ValidationT[F, B] =
    flatMap(f.andThen(ValidationT.apply))

  def transform[B](f: Validation[A] => Validation[B])(implicit F: Functor[F]): ValidationT[F, B] =
    ValidationT(F.map(value)(f))

  def subflatMap[B](f: A => Validation[B])(implicit F: Functor[F]): ValidationT[F, B] =
    transform(_.fold(_.invalid, f))

  def fold[C](fa: ValidationErrors => C, fb: A => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

  def ===(that: ValidationT[F, A])(implicit eq: Eq[F[Validation[A]]]): Boolean =
    eq.eqv(value, that.value)

  def partialCompare(that: ValidationT[F, A])(implicit p: PartialOrder[F[Validation[A]]]): Double =
    p.partialCompare(value, that.value)

  def toEitherT(implicit F: Functor[F]): EitherT[F, ValidationErrors, A] = EitherT(toEither)

  def toEither(implicit F: Functor[F]): F[Either[ValidationErrors, A]] = F.map(value)(_.toEither)
}

object ValidationT {
  def pure[F[_], A](a: A)(implicit F: Applicative[F]): ValidationT[F, A] = liftF(F.pure(a))

  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): ValidationT[F, A] = ValidationT(F.map(fa)(Valid(_)))

  def lift[F[_], A](va: Validation[A])(implicit F: Applicative[F]): ValidationT[F, A] = ValidationT(F.pure(va))
}
