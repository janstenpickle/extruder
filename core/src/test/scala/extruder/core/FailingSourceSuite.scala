package extruder.core

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.either._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, EitherValues, FunSuite, Matchers}
import shapeless.CNil

import scala.concurrent.duration.FiniteDuration

class FailingSourceSuite
    extends FunSuite
    with GeneratorDrivenPropertyChecks
    with EitherValues
    with Matchers
    with MapDecoders
    with MapEncoders {
  import FailingSourceSuite._
  import TestCommon._
  import ValidationCatsInstances._

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Map[String, String]] =
    if (data.keys.forall(_.contains(finalizeFailKey))) F.validationFailure(finalizeFailMessage)
    else F.pure(data)

  override protected def prepareInput[F[_]](namespace: List[String], settings: Sett, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Map[String, String]] =
    if (data.keys.forall(_.contains(prepareFailKey))) F.validationFailure(prepareFailMessage)
    else F.pure(data)

  override protected def lookupValue[F[_]](path: List[String], settings: Sett, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Option[String]] =
    if (path.contains(okNamespace)) F.pure(data.get(settings.pathToString(path)))
    else F.validationFailure(lookupFailMessage)

  override protected def writeValue[F[_]](path: List[String], settings: Sett, value: String)(
    implicit F: Eff[F]
  ): F[Map[String, String]] =
    if (path.contains(okNamespace)) F.validationFailure(writeFailMessage)
    else F.pure(Map(settings.pathToString(path) -> value))

  test("Lookup in the data source fails") { testLookupFail(Gen.alphaNumStr) }

  test("Writing to a data sink fails") { testWriteFail(Gen.alphaNumStr) }

  test("Lookup of an optional case class fails") { testLookupFail[Option[CC]](Gen.resultOf(CC).map(Some(_))) }

  test("The type specified for a sealed trait implementation is invalid") {
    assert(
      cnilDecoder[Validation].read(List.empty, defaultSettings, None, Map.empty) ===
        implicitly[Eff[Validation]].validationFailure(
          s"Could not find specified implementation of sealed type at path '${defaultSettings.pathToStringWithType(List.empty)}'"
        )
    )
  }

  test("The value specified for a duration is invalid") {
    assert(
      decode[FiniteDuration](List(okNamespace), Map(okNamespace -> "Inf")) === Left(
        NonEmptyList.of(
          ValidationFailure(
            s"Could not parse value 'Inf' at '$okNamespace': Could not parse value 'Inf' as a valid duration for type 'FiniteDuration'"
          )
        )
      )
    )
  }

  test("Cannot decode a char type") {
    forAll(Gen.alphaNumStr.suchThat(_.length > 1))(
      value =>
        assert(
          decode[Char](List(okNamespace), Map(okNamespace -> value)) === Left(
            NonEmptyList.of(ValidationFailure(s"Could not parse value '$value' at '$okNamespace': Not a valid Char"))
          )
      )
    )
  }

  test("Preparation of the data source fails") {
    forAll { value: String =>
      assert(
        decode[String](Map(prepareFailKey -> value)) === Left(NonEmptyList.of(ValidationFailure(prepareFailMessage)))
      )
    }
  }

  test("Finalization of the data sink fails") {
    forAll { value: String =>
      assert(
        encode[String](List(finalizeFailKey), value) === Left(NonEmptyList.of(ValidationFailure(finalizeFailMessage)))
      )
    }

  }

  def testLookupFail[T](
    gen: Gen[T]
  )(implicit encoder: MapEncoder[Validation, T], decoder: MapDecoder[Validation, T]): Assertion =
    test[T](gen, _ === NonEmptyList.of(ValidationFailure(lookupFailMessage)))

  def testWriteFail[T](
    gen: Gen[T]
  )(implicit encoder: MapEncoder[Validation, T], decoder: MapDecoder[Validation, T]): Assertion =
    test[T](gen, _ === NonEmptyList.of(ValidationFailure(writeFailMessage)), Some(okNamespace))

  def test[T](gen: Gen[T], expected: NonEmptyList[ValidationError] => Boolean, namespacePrefix: Option[String] = None)(
    implicit encoder: MapEncoder[Validation, T],
    decoder: MapDecoder[Validation, T]
  ): Assertion =
    forAll(gen, namespaceGen) { (value, namespace) =>
      val ns = namespacePrefix.fold(namespace)(namespace :+ _)
      assert(expected((for {
        encoded <- encode[T](ns, value)
        decoded <- decode[T](ns, encoded)
      } yield decoded).left.value))
    }

}

object FailingSourceSuite {
  val okNamespace = "lookupwillbesuccessful"
  val lookupFailMessage = "something went wrong with lookup"
  val writeFailMessage = "couldn't write"
  val prepareFailKey = "preparefail"
  val prepareFailMessage = "failure in preparing data"
  val finalizeFailKey = "finalizefail"
  val finalizeFailMessage = "failure in finalizing data"

  implicit val finiteDurationEq: Eq[FiniteDuration] = Eq.by(_.toMicros)
  implicit val cnilEq: Eq[CNil] = Eq.by(_.toString)

  case class CC(s: String)
}
