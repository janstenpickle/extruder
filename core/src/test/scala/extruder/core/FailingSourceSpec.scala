package extruder.core

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all._
import org.scalacheck.{Gen, Prop}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

import scala.concurrent.duration.FiniteDuration

class FailingSourceSpec extends Specification with ScalaCheck with EitherMatchers with MapDecoders with MapEncoders {
  import FailingSourceSpec._
  import TestCommon._

  override def is: SpecStructure =
    s2"""
       Returns an error when
        Lookup in the data source fails ${testLookupFail(Gen.alphaNumStr)}
        Writing to a data sink fails ${testWriteFail(Gen.alphaNumStr)}
        The type specified for a sealed trait implementation is invalid $testCnilDecoder
        The value specified for a duration is invalid $testDurationDecoder
        Cannot decode a char type $testCharDecoder
        Lookup of an optional case class fails ${testLookupFail[Option[CC]](Gen.resultOf(CC).map(Some(_)))}
        Preparation of the data source fails $testPrepareFail
        Finalization of the data sink fails $testFinalizeFail
      """

  override protected def finalizeOutput[F[_], E](
    namespace: List[String],
    data: Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    if (data.keys.forall(_.contains(finalizeFailKey))) IO.pure(AE.validationFailure(finalizeFailMessage))
    else IO.pure(AE.pure(data))

  override protected def prepareInput[F[_], E](
    namespace: List[String],
    data: Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    if (data.keys.forall(_.contains(prepareFailKey))) IO.pure(AE.validationFailure(prepareFailMessage))
    else IO.pure(AE.pure(data))

  override protected def lookupValue[F[_], E](
    path: List[String],
    data: Map[String, String]
  )(implicit hints: MapHints, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]] =
    if (path.contains(okNamespace)) IO.pure(AE.pure(data.get(hints.pathToString(path))))
    else IO.pure(AE.validationFailure(lookupFailMessage))

  override protected def writeValue[F[_], E](
    path: List[String],
    value: String
  )(implicit hints: MapHints, AE: ExtruderApplicativeError[F, E]): IO[F[Map[String, String]]] =
    if (path.contains(okNamespace)) IO.pure(AE.validationFailure(writeFailMessage))
    else IO.pure(AE.pure(Map(hints.pathToString(path) -> value)))

  def testLookupFail[T](
    gen: Gen[T]
  )(implicit encoder: MapEncoder[Validation, T], decoder: MapDecoder[Validation, T]): Prop =
    test[T](gen, _ mustEqual NonEmptyList.of(ValidationFailure(lookupFailMessage)))

  def testWriteFail[T](
    gen: Gen[T]
  )(implicit encoder: MapEncoder[Validation, T], decoder: MapDecoder[Validation, T]): Prop =
    test[T](gen, _ mustEqual NonEmptyList.of(ValidationFailure(writeFailMessage)), Some(okNamespace))

  def testCnilDecoder(
    implicit hints: MapHints,
    AE: ExtruderApplicativeError[Validation, NonEmptyList[ValidationError]]
  ): MatchResult[Any] =
    cnilDecoder.read(List.empty, None, Map.empty) mustEqual IO.pure(
      AE.validationFailure(
        s"Could not find specified implementation of sealed type at path '${hints.pathToStringWithType(List.empty)}'"
      )
    )

  def testDurationDecoder: MatchResult[Any] =
    decode[FiniteDuration](List(okNamespace), Map(okNamespace -> "Inf")).toEither must beLeft(
      NonEmptyList.of(
        ValidationFailure(
          s"Could not parse value 'Inf' at '$okNamespace': Could not parse value 'Inf' as a valid duration for type 'FiniteDuration'"
        )
      )
    )

  def testCharDecoder: Prop =
    Prop.forAllNoShrink(Gen.alphaNumStr.suchThat(_.length > 1))(
      value =>
        decode[Char](List(okNamespace), Map(okNamespace -> value)).toEither must beLeft(
          NonEmptyList.of(ValidationFailure(s"Could not parse value '$value' at '$okNamespace': Not a valid Char"))
      )
    )

  def testPrepareFail: Prop =
    Prop.forAll(Gen.alphaNumStr)(
      value =>
        decode[String](Map(prepareFailKey -> value)).toEither must beLeft(
          NonEmptyList.of(ValidationFailure(prepareFailMessage))
      )
    )

  def testFinalizeFail: Prop =
    Prop.forAll(Gen.alphaNumStr)(
      value =>
        encode[String](List(finalizeFailKey), value).toEither must beLeft(
          NonEmptyList.of(ValidationFailure(finalizeFailMessage))
      )
    )

  def test[T](
    gen: Gen[T],
    expected: NonEmptyList[ValidationError] => MatchResult[Any],
    namespacePrefix: Option[String] = None
  )(implicit encoder: MapEncoder[Validation, T], decoder: MapDecoder[Validation, T]): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      val ns = namespacePrefix.fold(namespace)(namespace :+ _)
      (for {
        encoded <- encode[T](ns, value).toEither
        decoded <- decode[T](ns, encoded).toEither
      } yield decoded) must beLeft.which(expected)
    }
}

object FailingSourceSpec {
  val okNamespace = "lookupwillbesuccessful"
  val lookupFailMessage = "something went wrong with lookup"
  val writeFailMessage = "couldn't write"
  val prepareFailKey = "preparefail"
  val prepareFailMessage = "failure in preparing data"
  val finalizeFailKey = "finalizefail"
  val finalizeFailMessage = "failure in finalizing data"
  case class CC(s: String)
}
