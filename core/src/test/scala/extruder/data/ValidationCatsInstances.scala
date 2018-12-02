package extruder.data

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Cogen, Gen}

object ValidationCatsInstances {
  implicit val validationErrorsArb: Arbitrary[ValidationErrors] = Arbitrary(
    Gen
      .nonEmptyListOf(
        Gen.oneOf(
          Gen.alphaNumStr.map(Missing(_)),
          Gen.alphaNumStr.map(ValidationFailure(_)),
          Gen.alphaNumStr.map(str => ValidationException(new RuntimeException(str)))
        )
      )
      .map(l => NonEmptyList.of(l.head, l.tail: _*))
  )

  implicit def validationEq[A: Eq]: Eq[Validation[A]] = Eq.by(_.a)

  implicit def validationTEq[F[_], A](implicit eq: Eq[F[Either[ValidationErrors, A]]]): Eq[ValidationT[F, A]] =
    Eq.by(_.a)

  implicit val validationErrorsEq: Eq[ValidationError] = Eq.by(_.toString)

  implicit val vCogen: Cogen[ValidationErrors] = Cogen[String].contramap(_.toString)

  implicit val thEq: Eq[Throwable] = Eq.by(_.getMessage)
}
