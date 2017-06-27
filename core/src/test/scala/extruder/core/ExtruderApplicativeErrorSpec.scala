package extruder.core

import cats.Eq
import cats.data.NonEmptyList
import cats.effect.IO
import cats.instances.all._
import cats.laws.discipline.ApplicativeErrorTests
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

abstract class ExtruderApplicativeErrorSpec[F[_], E](
  implicit AE: ExtruderApplicativeError[F, E],
  eArb: Arbitrary[E],
  cogen: Cogen[E],
  eEq: Eq[E]
) extends Specification
    with ScalaCheck
    with Discipline {
  implicit val hints: Hints = new Hints {
    override def pathToString(path: List[String]): String = path.mkString(".").toLowerCase
  }

  def is: SpecStructure =
    s2"""
        Handles non fatal exceptions $testCatchNonFatal
        Can evaluate an IO $testAttemptIO
        Can handle the following types of error
          Missing $testMissing
          Validation failure $testValidationFailure
          Validation exception $testValidationException
        Can append errors $testAppendErrors
        ${checkAll("ExtruderApplicativeError", tests.applicativeError[Int, Int, Int])}
      """

  def missingValue(message: String): E
  def validationFailureValue(message: String): E
  def validationExceptionValue(message: String, th: Throwable): E
  def compareErrors[A](f: F[A], e1: Option[E], e2: Option[E])(implicit e: Eq[F[A]]): Boolean

  lazy val tests: ApplicativeErrorTests[F, E] = ApplicativeErrorTests[F, E]

  implicit def FArb[A](implicit arb: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(arb.arbitrary.map(a => AE.pure(a)))
  implicit def feq[A](implicit eq: Eq[A]): Eq[F[A]]

  def testCatchNonFatal(implicit eq: Eq[F[String]]): Prop = prop { str: String =>
    val th = new RuntimeException(str)
    eq.eqv(AE.catchNonFatal(str), AE.pure(str))
      .and(eq.eqv(AE.catchNonFatal(throw th), AE.validationException(str, th)))
  }

  def testAttemptIO(implicit eq: Eq[F[Int]], hints: Hints): Prop = prop { i: Int =>
    eq.eqv(AE.attemptIO(IO(AE.pure(i))), AE.pure(i))
  }

  def testMissing(implicit eq: Eq[F[Int]]): Prop = prop { str: String =>
    eq.eqv(AE.missing(str), AE.raiseError(missingValue(str)))
  }

  def testValidationFailure(implicit eq: Eq[F[Int]]): Prop = prop { str: String =>
    eq.eqv(AE.validationFailure(str), AE.raiseError(validationFailureValue(str)))
  }

  def testValidationException(implicit eq: Eq[F[Int]]): Prop = prop { (str: String, thMsg: String) =>
    val th = new RuntimeException(thMsg)
    eq.eqv(AE.validationException(str, th), AE.raiseError(validationExceptionValue(str, th)))
  }

  def testAppendErrors: Prop = prop { (err1: String, err2: String) =>
    val missing1 = missingValue(err1)
    val missing2 = missingValue(err2)
    compareErrors(AE.ap(AE.missing[Int => Int](err1))(AE.missing[Int](err2)), Some(missing1), Some(missing2)) &&
    compareErrors(AE.ap(AE.pure[Int => Int](identity))(AE.missing[Int](err2)), None, Some(missing2)) &&
    compareErrors(AE.ap(AE.missing[Int => Int](err1))(AE.pure(1)), Some(missing1), None) &&
    compareErrors(AE.ap(AE.pure[Int => Int](identity))(AE.pure(1)), None, None)
  }
}

abstract class ExtruderApplicativeErrorThrowableSpec[F[_]](implicit AE: ExtruderApplicativeError[F, Throwable])
    extends ExtruderApplicativeErrorSpec[F, Throwable]()(
      AE,
      Arbitrary[Throwable](implicitly[Arbitrary[Exception]].arbitrary),
      implicitly[Cogen[Throwable]],
      Eq[String].on(_.toString)
    ) {
  override def missingValue(message: String): Throwable = new NoSuchElementException(message)
  override def validationFailureValue(message: String): Throwable = new RuntimeException(message)
  override def validationExceptionValue(message: String, th: Throwable): Throwable = th
  override def compareErrors[A](f: F[A], th1: Option[Throwable], th2: Option[Throwable])(
    implicit e: Eq[F[A]]
  ): Boolean = {
    val err: F[A] = (th1, th2) match {
      case (Some(e1), Some(e2)) => e1.addSuppressed(e2); AE.raiseError(e1)
      case (Some(e1), None) => AE.raiseError(e1)
      case (None, Some(e2)) => AE.raiseError(e2)
      case _ => f
    }
    e.eqv(f, err)
  }
}

abstract class ExtruderApplicativeErrorValidationErrorsSpec[F[_]](
  implicit AE: ExtruderApplicativeError[F, ValidationErrors]
) extends ExtruderApplicativeErrorSpec[F, ValidationErrors]()(
      AE,
      ConfigValidationCatsInstances.validationErrorsArb,
      ConfigValidationCatsInstances.vCogen,
      Eq.fromUniversalEquals
    ) {
  override def missingValue(message: String): ValidationErrors = NonEmptyList.of(Missing(message))
  override def validationFailureValue(message: String): ValidationErrors = NonEmptyList.of(ValidationFailure(message))
  override def validationExceptionValue(message: String, th: Throwable): ValidationErrors =
    NonEmptyList.of(ValidationException(message, th))
  override implicit def feq[A](implicit eq: Eq[A]): Eq[F[A]] = Eq.fromUniversalEquals

  override def compareErrors[A](f: F[A], v1: Option[ValidationErrors], v2: Option[ValidationErrors])(
    implicit e: Eq[F[A]]
  ): Boolean = {
    val err: F[A] = (v1, v2) match {
      case (Some(e1), Some(e2)) => AE.raiseError(e1 ++ e2.toList)
      case (Some(e1), None) => AE.raiseError(e1)
      case (None, Some(e2)) => AE.raiseError(e2)
      case _ => f
    }
    e.eqv(f, err)
  }
}
