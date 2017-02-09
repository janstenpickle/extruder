package extruder.core

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import org.scalacheck.{Gen, Prop}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import shapeless.{Coproduct, Inl, Inr, LabelledGeneric}

object ResolversBaseSpec {
  sealed trait Sealed
  case object Obj1 extends Sealed
  case object Obj2 extends Sealed
}

class ResolversBaseSpec extends Specification with ScalaCheck with EitherMatchers with ResolversBase {
  import ResolversBaseSpec._

  override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase

  override def is: SpecStructure = s2"""
    Adds the type key to a path for when resolving sealed families $testTypeKey
    Creates a resolver which always returns an error in the event of a `CNil` when resolving sealed families $testCNil
    Correctly formats an error message $testErrorMsg

    Ccons resolver
      Can resolve `Obj1` $testCconsObj1
      Can resolve `Obj2` $testCconsObj2
      Fails to resolve when the resolver for `Obj1` returns an error $testCconsObj1Fail
      Fails to resolve when the resolver for `Obj2` returns an error $testCconsObj2Fail
      Returns an empty option when the type cannot be resolved $testCconsObj1Empty
  """

  def testTypeKey: Prop = Prop.forAll(Gen.listOf(Gen.alphaNumStr))(path =>
    pathToStringWithType(path) === pathToString(path :+ typeKey)
  )

  def testCNil: Prop = Prop.forAll(Gen.listOf(Gen.alphaNumStr))(path =>
    cnil.read(path, None).toEither must beLeft.which(err =>
      err.toList.size === 1 and
      err.head.message === s"Could not find specified implementation of sealed type at configuration path '${pathToStringWithType(path)}'"
    )
  )

  def testErrorMsg: Prop = Prop.forAll(Gen.listOf(Gen.alphaNumStr))(path =>
    errorMsg[String](path).toEither must beLeft.which(err =>
      err.toList.size === 1 and
      err.head.message === s"Could not find configuration at '${pathToString(path)}' and no default available"
    )
  )

  def testCconsObj1: MatchResult[Any] = {
    implicit val obj1Res: Resolver[Option[Obj1.type]] = Resolver(Valid(Some(Obj1)))
    implicit val obj2Res: Resolver[Option[Obj2.type]] = Resolver(Valid(Some(Obj2)))
    implicit val typeRes: Resolver[Option[String]] = Resolver(Valid(Some("Obj1")))

    testCcons must beRight(Some(Inl(Obj1)))
  }

  def testCconsObj2: MatchResult[Any] = {
    implicit val obj1Res: Resolver[Option[Obj1.type]] = Resolver(Valid(Some(Obj1)))
    implicit val obj2Res: Resolver[Option[Obj2.type]] = Resolver(Valid(Some(Obj2)))
    implicit val typeRes: Resolver[Option[String]] = Resolver(Valid(Some("Obj2")))

    testCcons must beRight(Some(Inr(Inl(Obj2))))
  }

  def testCconsObj1Fail: MatchResult[Any] = {
    implicit val obj1Res: Resolver[Option[Obj1.type]] = Resolver(ValidationFailure("Fail"))
    implicit val obj2Res: Resolver[Option[Obj2.type]] = Resolver(Valid(Some(Obj2)))
    implicit val typeRes: Resolver[Option[String]] = Resolver(Valid(Some("Obj1")))

    testCcons must beLeft.which(err =>
      err.toList.size === 1 and
      err.head.message === "Fail"
    )
  }

  def testCconsObj2Fail: MatchResult[Any] = {
    implicit val obj1Res: Resolver[Option[Obj1.type]] = Resolver(Valid(Some(Obj1)))
    implicit val obj2Res: Resolver[Option[Obj2.type]] = Resolver(ValidationFailure("Fail"))
    implicit val typeRes: Resolver[Option[String]] = Resolver(Valid(Some("Obj2")))

    testCcons must beLeft.which(err =>
      err.toList.size === 1 and
      err.head.message === "Fail"
    )
  }

  def testCconsObj1Empty: MatchResult[Any] = {
    implicit val obj1Res: Resolver[Option[Obj1.type]] = Resolver(Valid(Some(Obj1)))
    implicit val obj2Res: Resolver[Option[Obj2.type]] = Resolver(Valid(Some(Obj2)))
    implicit val typeRes: Resolver[Option[String]] = Resolver(Valid(None))

    testCcons must beRight.which(_.isEmpty)
  }

  def testCcons[C <: Coproduct](implicit gen: LabelledGeneric.Aux[Sealed, C],
                                underlying: Resolver[Option[C]]): Either[NonEmptyList[ValidationFailure], Option[C]] =
    underlying.read(Seq.empty, None).toEither
}
