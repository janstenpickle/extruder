package extruder.core

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class ConfigSourceSpec extends Specification with ScalaCheck with ConfigSource {
  import ConfigSourceSpec._

  override def is: SpecStructure =
    s2"""
        Converts a non-empty list of validation errors to throwables $testErrorsToThrowable
      """

  def testErrorsToThrowable: Prop = prop { li: ValidationErrors =>
    val th = errorsToThrowable(li)
    li.map(_.message).toList === (th :: th.getSuppressed.toList).map(_.getMessage)
  }
}

object ConfigSourceSpec {
  implicit val validationErrorsArb: Arbitrary[ValidationErrors] = Arbitrary(
    Gen
      .nonEmptyListOf(Gen.alphaNumStr)
      .map(
        li =>
          NonEmptyList
            .of(li.head, li.tail: _*)
            .flatMap[ValidationError](
              msg => NonEmptyList.of(ValidationFailure(msg), ValidationException(msg, new RuntimeException(msg)))
          )
      )
  )
}
