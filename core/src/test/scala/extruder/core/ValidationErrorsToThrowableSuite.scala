package extruder.core

import cats.data.NonEmptyList
import extruder.core.ValidationErrorsToThrowable.defaultValidationErrorsThrowable
import extruder.data.{ValidationError, ValidationErrors}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValidationErrorsToThrowableSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  import ValidationErrorsToThrowableSuite._

  test("Converts a non-empty list of validation errors to throwables") {
    forAll { li: ValidationErrors =>
      val th = defaultValidationErrorsThrowable.convertErrors(li)
      assert(li.map(_.message).toList === (th :: th.getSuppressed.toList).map(_.getMessage))
    }
  }
}

object ValidationErrorsToThrowableSuite {
  implicit val validationErrorsGen: Arbitrary[ValidationErrors] =
    Arbitrary(
      for {
        head <- Gen.alphaNumStr
        tail <- Gen.listOf(Gen.alphaNumStr)
      } yield
        NonEmptyList
          .of(head, tail: _*)
          .flatMap(
            msg =>
              NonEmptyList.of(
                ValidationError.failure(msg),
                ValidationError.missing(msg),
                ValidationError.exception(msg, new RuntimeException(msg))
            )
          )
    )
}
