package extruder.core

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.all._
import org.scalacheck.{Arbitrary, Cogen, Gen}

object ValidationCatsInstances {
  implicit val validationErrorsArb: Arbitrary[ValidationErrors] = Arbitrary(
    Gen
      .nonEmptyListOf(
        Gen.oneOf(
          Gen.alphaNumStr.map(Missing(_)),
          Gen.alphaNumStr.map(ValidationFailure(_)),
          Gen.alphaNumStr.map(str => ValidationException(str, new RuntimeException(str)))
        )
      )
      .map(l => NonEmptyList.of(l.head, l.tail: _*))
  )

  implicit def validationEq[A]: Eq[Validation[A]] = Eq.by[Validation[A], String](_.toString)

  implicit val validationErrorsEq: Eq[ValidationError] = Eq.by(_.toString)

  implicit val vCogen: Cogen[ValidationErrors] = Cogen[String].contramap(_.toString)
}
