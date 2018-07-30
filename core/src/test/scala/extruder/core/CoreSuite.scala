package extruder.core

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CoreSuite extends FunSuite with GeneratorDrivenPropertyChecks with DataSource {
  import CoreSuite._

  override type Sett = Settings

  test("Converts a non-empty list of validation errors to throwables") {
    forAll { li: ValidationErrors =>
      val th = errorsToThrowable(li)
      li.map(_.message).toList === (th :: th.getSuppressed.toList).map(_.getMessage)
    }
  }

  override def defaultSettings: Sett = new Sett {
    override def pathToString(path: List[String]): String = path.mkString(".")
  }
}

object CoreSuite {
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
