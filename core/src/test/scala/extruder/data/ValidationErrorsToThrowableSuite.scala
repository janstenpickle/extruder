package extruder.data

import cats.data.NonEmptyList
import extruder.data.ValidationErrorsToThrowable.defaultValidationErrorsThrowable
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ValidationErrorsToThrowableSuite extends FunSuite with GeneratorDrivenPropertyChecks {
  import ValidationErrorsToThrowableSuite._

  test("Converts a non-empty list of validation errors to throwables") {
    forAll { li: ValidationErrors =>
      val th = defaultValidationErrorsThrowable.convertErrors(li)
      li.map(_.message).toList === (th :: th.getSuppressed.toList).map(_.getMessage)
    }
  }
}

object ValidationErrorsToThrowableSuite {
  implicit val validationErrorsArb: Arbitrary[ValidationErrors] = Arbitrary(
    Gen
      .nonEmptyListOf(Gen.alphaNumStr)
      .map(
        li =>
          NonEmptyList
            .of(li.head, li.tail: _*)
            .flatMap[ValidationError](
              msg =>
                NonEmptyList.of(
                  ValidationError.failure(msg),
                  ValidationError.missing(msg),
                  ValidationError.exception(msg, new RuntimeException(msg))
              )
          )
      )
  )
}
