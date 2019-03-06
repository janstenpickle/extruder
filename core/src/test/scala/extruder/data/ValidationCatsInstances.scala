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
          Gen.alphaNumStr.map(ValidationError.missing),
          Gen.alphaNumStr.map(ValidationError.failure),
          Gen.alphaNumStr.map(str => ValidationError.exception(new RuntimeException(str)))
        )
      )
      .map(l => NonEmptyList.of(l.head, l.tail: _*))
  )

  implicit def arb[A](implicit arb: Arbitrary[A]): Arbitrary[Validation[A]] =
    Arbitrary(arb.arbitrary.map(a => Validation(Right(a))))

  implicit def validationEq[A: Eq]: Eq[Validation[A]] = Eq.by(_.a)

  implicit val validationErrorsEq: Eq[ValidationError] = Eq.by(_.toString)

  implicit val thEq: Eq[Throwable] = Eq.by(_.getMessage)
}
