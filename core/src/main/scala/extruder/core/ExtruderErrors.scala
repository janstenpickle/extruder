package extruder.core
import cats.{Applicative, MonadError}
import cats.data.{EitherT, NonEmptyList}

trait ExtruderErrors[F[_]] {
  def missing[A](message: String): F[A]
  def validationFailure[A](message: String): F[A]
  def validationException[A](message: String, ex: Throwable): F[A]
}

trait LowPriorityInstances {
  implicit def monadError[F[_]](implicit F: MonadError[F, Throwable]): ExtruderErrors[F] = new ExtruderErrors[F] {
    override def missing[A](message: String): F[A] = F.raiseError(new NoSuchElementException(message))
    override def validationFailure[A](message: String): F[A] = F.raiseError(new RuntimeException(message))
    override def validationException[A](message: String, ex: Throwable): F[A] = F.raiseError(ex)
  }
}

object ExtruderErrors extends LowPriorityInstances {
  implicit def eitherT[F[_]](
    implicit F: MonadError[EitherT[F, ValidationErrors, ?], Throwable],
    FF: Applicative[F]
  ): ExtruderErrors[EitherT[F, ValidationErrors, ?]] = new ExtruderErrors[EitherT[F, ValidationErrors, ?]] {
    override def missing[A](message: String): EitherT[F, ValidationErrors, A] =
      EitherT.leftT[F, A](NonEmptyList.of[ValidationError](Missing(message)))

    override def validationFailure[A](message: String): EitherT[F, ValidationErrors, A] =
      EitherT.leftT[F, A](NonEmptyList.of[ValidationError](ValidationFailure(message)))

    override def validationException[A](message: String, ex: Throwable): EitherT[F, ValidationErrors, A] =
      EitherT.leftT[F, A](NonEmptyList.of[ValidationError](ValidationException(message, ex)))
  }

  implicit val validation: ExtruderErrors[Validation] = new ExtruderErrors[Validation] {
    override def missing[A](message: String): Validation[A] = Left(NonEmptyList.of(Missing(message)))
    override def validationFailure[A](message: String): Validation[A] =
      Left(NonEmptyList.of(ValidationFailure(message)))
    override def validationException[A](message: String, ex: Throwable): Validation[A] =
      Left(NonEmptyList.of(ValidationException(message, ex)))
  }

  def apply[F[_]](implicit value: ExtruderErrors[F]): ExtruderErrors[F] = value
}
