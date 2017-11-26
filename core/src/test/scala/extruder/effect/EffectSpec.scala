package extruder.effect

import cats.Eq
import cats.instances.all._
import org.scalacheck.{Arbitrary, Prop}
import org.specs2.specification.core.{Fragments, SpecStructure}
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

abstract class EffectSpec[F[_], E](implicit F: ExtruderMonadError[F])
    extends Specification
    with ScalaCheck
    with Discipline {

  override def is: SpecStructure =
    s2"""
        Can handle the following types of error
          Missing $testMissing
          Validation failure $testValidationFailure
          Validation exception $testValidationException
        Can append errors $testAppendErrors
        $ruleTest
      """

  def ruleTest: Fragments

  implicit val throwableEq: Eq[Throwable] = Eq.by(_.toString)
  implicit def feq[A](implicit eq: Eq[A]): Eq[F[A]]
  implicit def FArb[A](implicit arb: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(arb.arbitrary.map(a => F.pure(a)))

  def errEq: Eq[E]
  def missingValue(message: String): E
  def validationFailureValue(message: String): E
  def validationExceptionValue(message: String, th: Throwable): E

  def compareErrors[A](f: F[A], e1: Option[E], e2: Option[E])(implicit e: Eq[A]): Boolean

  def getError[A](fa: F[A]): E

  def testMissing: Prop = prop { str: String =>
    errEq.eqv(getError(F.missing(str)), missingValue(str))
  }

  def testValidationFailure: Prop = prop { str: String =>
    errEq.eqv(getError(F.validationFailure(str)), validationFailureValue(str))
  }

  def testValidationException: Prop = prop { (str: String, thMsg: String) =>
    val th = new RuntimeException(thMsg)
    errEq.eqv(getError(F.validationException(str, th)), validationExceptionValue(str, th))
  }

  def testAppendErrors: Prop = prop { (err1: String, err2: String) =>
    val missing1 = missingValue(err1)
    val missing2 = missingValue(err2)
    compareErrors(F.ap(F.missing[Int => Int](err1))(F.missing[Int](err2)), Some(missing1), Some(missing2)) &&
    compareErrors(F.ap(F.pure[Int => Int](identity))(F.missing[Int](err2)), None, Some(missing2)) &&
    compareErrors(F.ap(F.missing[Int => Int](err1))(F.pure(1)), Some(missing1), None) &&
    compareErrors(F.ap(F.pure[Int => Int](identity))(F.pure(1)), None, None)
  }
}
