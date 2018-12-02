package extruder.core

import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Eq, MonadError}
import extruder.data.{Validation, ValidationError, ValidationFailure}
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

  override type DecEff[F[_]] = MonadError[F, Throwable]
  override type EncEff[F[_]] = MonadError[F, Throwable]

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, data: Map[String, String])(
    implicit F: EncEff[F]
  ): F[Map[String, String]] =
    if (data.keys.forall(_.contains(finalizeFailKey))) F.raiseError(new RuntimeException(finalizeFailMessage))
    else F.pure(data)

  override protected def prepareInput[F[_]](namespace: List[String], settings: Sett, data: Map[String, String])(
    implicit F: DecEff[F]
  ): F[Map[String, String]] =
    if (data.keys.forall(_.contains(prepareFailKey))) F.raiseError(new RuntimeException(prepareFailMessage))
    else F.pure(data)

  override protected def lookupValue[F[_]](path: List[String], settings: Sett, data: Map[String, String])(
    implicit F: DecEff[F]
  ): F[Option[String]] =
    if (path.contains(okNamespace)) F.pure(data.get(settings.pathToString(path)))
    else F.raiseError(new RuntimeException(lookupFailMessage))

  override protected def writeValue[F[_]](path: List[String], settings: Sett, value: String)(
    implicit F: EncEff[F]
  ): F[Map[String, String]] =
    if (path.contains(okNamespace)) F.raiseError(new RuntimeException(writeFailMessage))
    else F.pure(Map(settings.pathToString(path) -> value))

  test("Lookup in the data source fails") { testLookupFail(Gen.alphaNumStr) }

  test("Writing to a data sink fails") { testWriteFail(Gen.alphaNumStr) }

  test("Lookup of an optional case class fails") { testLookupFail[Option[CC]](Gen.resultOf(CC).map(Some(_))) }

  test("The type specified for a sealed trait implementation is invalid") {
    assert(
      cnilDecoder[Validation].read(List.empty, defaultSettings, None, Map.empty) ===
        implicitly[ExtruderErrors[Validation]].validationFailure(
          s"Could not find specified implementation of sealed type at path '${defaultSettings.pathToStringWithType(List.empty)}'"
        )
    )
  }

  test("The value specified for a duration is invalid") {
    assert(
      decodeF[Validation, FiniteDuration](List(okNamespace), Map(okNamespace -> "Inf")) === Left(
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
          decodeF[Validation, Char](List(okNamespace), Map(okNamespace -> value)) === Left(
            NonEmptyList.of(ValidationFailure(s"Could not parse value '$value' at '$okNamespace': Not a valid Char"))
          )
      )
    )
  }

  test("Preparation of the data source fails") {
    forAll { value: String =>
      assert(decodeF[Validation, String](Map(prepareFailKey -> value)).left.value.head.message === prepareFailMessage)
    }
  }

  test("Finalization of the data sink fails") {
    forAll { value: String =>
      assert(encodeF[Validation](List(finalizeFailKey), value).left.value.head.message === finalizeFailMessage)
    }

  }

  def testLookupFail[T](
    gen: Gen[T]
  )(implicit encoder: MapEncoder[Validation, T], decoder: MapDecoder[Validation, T]): Assertion =
    test[T](gen, _.head.message === lookupFailMessage)

  def testWriteFail[T](
    gen: Gen[T]
  )(implicit encoder: MapEncoder[Validation, T], decoder: MapDecoder[Validation, T]): Assertion =
    test[T](gen, _.head.message === writeFailMessage, Some(okNamespace))

  def test[T](gen: Gen[T], expected: NonEmptyList[ValidationError] => Boolean, namespacePrefix: Option[String] = None)(
    implicit encoder: MapEncoder[Validation, T],
    decoder: MapDecoder[Validation, T]
  ): Assertion =
    forAll(gen, namespaceGen) { (value, namespace) =>
      val ns = namespacePrefix.fold(namespace)(namespace :+ _)
      assert(expected((for {
        encoded <- encodeF[Validation](ns, value)
        decoded <- decodeF[Validation, T](ns, encoded)
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
