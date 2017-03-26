package extruder.core

import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.validated._
import org.scalacheck.{Gen, Prop}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure

import scala.concurrent.duration.FiniteDuration

class FailingConfigSpec extends Specification with ScalaCheck with EitherMatchers with MapDecoders with MapEncoders {
  import FailingConfigSpec._
  import TestCommon._

  override def is: SpecStructure =
    s2"""
       Returns an error when
        Lookup in the configuration source fails ${testLookupFail(Gen.alphaNumStr)}
        Writing to a configuration sink fails ${testWriteFail(Gen.alphaNumStr)}
        The type specified for a sealed trait implementation is invalid $testCnilDecoder
        The value specified for a duration is invalid $testDurationDecoder
        Cannot decode a char type $testCharDecoder
        Lookup of an optional case class fails ${testLookupFail[Option[CC]](Gen.resultOf(CC).map(Some(_)))}
        Preparation of the configuration source fails $testPrepareFail
        Finalization of the configuration sink fails $testFinalizeFail
      """

  override protected def finalizeConfig(config: Map[String, String]): ConfigValidation[Map[String, String]] =
    if (config.keys.forall(_.contains(finalizeFailKey))) ValidationFailure(finalizeFailMessage).invalidNel
    else config.validNel

  override protected def prepareConfig(config: Map[String, String]): ConfigValidation[Map[String, String]] =
    if (config.keys.forall(_.contains(prepareFailKey))) ValidationFailure(prepareFailMessage).invalidNel
    else config.validNel

  override protected def lookupValue(path: Seq[String], config: Map[String, String]): ConfigValidation[Option[String]] =
    if (path.contains(okNamespace)) config.get(pathToString(path)).validNel
    else ValidationFailure(lookupFailMessage).invalidNel

  override protected def writeValue(path: Seq[String], value: String): ConfigValidation[Map[String, String]] =
    if (path.contains(okNamespace)) ValidationFailure(writeFailMessage).invalidNel
    else Map(pathToString(path) -> value).validNel


  def testLookupFail[T](gen: Gen[T])
                       (implicit encoder: MapEncoder[T],
                        decoder: MapDecoder[T]): Prop =
    test[T](gen, _ mustEqual NonEmptyList.of(ValidationFailure(lookupFailMessage)))

  def testWriteFail[T](gen: Gen[T])
                      (implicit encoder: MapEncoder[T],
                       decoder: MapDecoder[T]): Prop =
    test[T](gen, _ mustEqual NonEmptyList.of(ValidationFailure(writeFailMessage)), Some(okNamespace))

  def testCnilDecoder: MatchResult[Any] =
    cnilDecoder.read(Seq.empty, None, Map.empty) mustEqual ValidationFailure(
      s"Could not find specified implementation of sealed type at configuration path '${pathToStringWithType(Seq.empty)}'"
    ).invalidNel

  def testDurationDecoder: MatchResult[Any] =
    decode[FiniteDuration](Seq(okNamespace), Map(okNamespace -> "Inf")).toEither must beLeft(NonEmptyList.of(
      ValidationFailure(s"Could not parse value 'Inf' at '$okNamespace': Could not parse value 'Inf' as a valid duration for type 'FiniteDuration'")
    ))

  def testCharDecoder: Prop = Prop.forAllNoShrink(Gen.alphaNumStr.suchThat(_.length > 1))(value =>
    decode[Char](Seq(okNamespace), Map(okNamespace -> value)).toEither must beLeft(NonEmptyList.of(
      ValidationFailure(s"Could not parse value '$value' at '$okNamespace': Not a valid Char")
    ))
  )

  def testPrepareFail(implicit decoder: MapDecoder[String]): Prop = Prop.forAll(Gen.alphaNumStr)(value =>
    decode[String](Map(prepareFailKey -> value)).toEither must beLeft(NonEmptyList.of(ValidationFailure(prepareFailMessage)))
  )

  def testFinalizeFail(implicit encoder: MapEncoder[String]): Prop = Prop.forAll(Gen.alphaNumStr)(value =>
    encode[String](Seq(finalizeFailKey), value).toEither must beLeft(NonEmptyList.of(ValidationFailure(finalizeFailMessage)))
  )

  def test[T](gen: Gen[T],
              expected: NonEmptyList[ValidationError] => MatchResult[Any],
              namespacePrefix: Option[String] = None)
             (implicit encoder: MapEncoder[T], decoder: MapDecoder[T]): Prop =
    Prop.forAll(gen, namespaceGen) { (value, namespace) =>
      val ns = namespacePrefix.fold(namespace)(namespace :+ _)
      (for {
        encoded <- encode[T](ns, value).toEither
        decoded <- decode[T](ns, encoded).toEither
      } yield decoded) must beLeft.which(expected)
    }
}

object FailingConfigSpec {
  val okNamespace = "lookupwillbesuccessful"
  val lookupFailMessage = "something went wrong with lookup"
  val writeFailMessage = "couldn't write"
  val prepareFailKey = "preparefail"
  val prepareFailMessage = "failure in preparing config"
  val finalizeFailKey = "finalizefail"
  val finalizeFailMessage = "failure in finalizing config"
  case class CC(s: String)
}
