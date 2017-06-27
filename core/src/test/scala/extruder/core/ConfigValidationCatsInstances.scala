package extruder.core

import cats.Eq
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.instances.all._

object ConfigValidationCatsInstances {
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

  implicit def validationErrorsEq[A](implicit aEq: Eq[A]): Eq[ConfigValidation[A]] = new Eq[ConfigValidation[A]] {
    override def eqv(x: ConfigValidation[A], y: ConfigValidation[A]): Boolean = (x, y) match {
      case (Valid(xx), Valid(yy)) => aEq.eqv(xx, yy)
      case (Invalid(xx), Invalid(yy)) => Eq[String].on[ValidationErrors](_.toString).eqv(xx, yy)
      case _ => false
    }
  }
  implicit def eitherErrorsEq[A]: Eq[EitherErrors[A]] = Eq[String].on(_.toString)

  implicit val vCogen: Cogen[ValidationErrors] = Cogen[String].contramap(_.toString)
}
