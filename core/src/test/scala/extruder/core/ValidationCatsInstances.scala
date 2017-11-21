package extruder.core

import cats.Eq
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.instances.all._

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

//  implicit def validationErrorsEq[A](implicit aEq: Eq[A]): Eq[Validation[A]] = new Eq[Validation[A]] {
//    override def eqv(x: Validation[A], y: Validation[A]): Boolean = (x, y) match {
//      case (Right(xx), Right(yy)) => aEq.eqv(xx, yy)
//      case (Left(xx), Left(yy)) => Eq.by[ValidationErrors, String](_.toString).eqv(xx, yy)
//      case _ => false
//    }
//  }
  implicit def eitherErrorsEq[A]: Eq[EitherErrors[A]] = Eq.by[EitherErrors[A], String](_.toString)

  implicit val vCogen: Cogen[ValidationErrors] = Cogen[String].contramap(_.toString)
}
