package extruder.instances

import cats.Eq
import cats.instances.all._
import cats.laws.discipline.CartesianTests.Isomorphisms
import cats.laws.discipline.FlatMapTests
import cats.syntax.validated._
import extruder.core.ConfigValidationCatsInstances._
import extruder.core.{ConfigValidation, ValidationError}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

class ConfigValidationFlatMapSpec extends Specification with Discipline with ConfigValidationInstances {
  implicit val validationErrorEq: Eq[ValidationError] = Eq[String].on(_.toString)
  implicit def iso: Isomorphisms[ConfigValidation] = Isomorphisms.invariant[ConfigValidation](configValidationFlatMap)
  implicit def FaArb[A](implicit tarb: Arbitrary[A]): Arbitrary[ConfigValidation[A]] =
    Arbitrary(Gen.oneOf(validationErrorsArb.arbitrary.map(_.invalid), tarb.arbitrary.map(_.valid)))
  val test: FlatMapTests[ConfigValidation] = FlatMapTests[ConfigValidation]

  override def is: SpecStructure =  checkAll("ConfigValidation FlatMap instance", test.flatMap[Int, Int, Int] )
}
