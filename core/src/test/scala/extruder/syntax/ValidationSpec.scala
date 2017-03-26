package extruder.syntax

import cats.data.NonEmptyList
import extruder.core.ValidationException
import extruder.syntax.validation._
import org.scalacheck.{Gen, Prop}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class ValidationSpec extends Specification with ScalaCheck {
  import ValidationSpec._

  override def is: SpecStructure =
    s2"""
        Encloses values in validated $valid
        Handles uncaught exceptions $invalid
      """

  def valid: Prop = Prop.forAll(Gen.alphaNumStr)(value =>
    value.handle.toEither must beRight(value)
  )

  def invalid: Prop = Prop.forAll(exceptionGen)(ex =>
    failingTest(ex).handle.toEither must beLeft(NonEmptyList.of(new ValidationException(ex.getMessage, ex)))
  )
}

object ValidationSpec {
  val exceptionGen: Gen[Throwable] = Gen.alphaNumStr.map(new RuntimeException(_))
  def failingTest(ex: Throwable): Unit = throw ex
}
