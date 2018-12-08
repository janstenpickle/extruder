//package extruder.core
//
//import cats.{Applicative, Eq}
//import org.scalacheck.Arbitrary
//import org.scalatest.FunSuite
//import org.scalatest.prop.GeneratorDrivenPropertyChecks
//import org.typelevel.discipline.scalatest.Discipline
//import cats.laws._
//
//abstract class ExtruderErrorsSuite[F[_]: Applicative: ExtruderErrors, A: Arbitrary: Eq](
//  implicit arbFa: Arbitrary[F[A]],
//  eqFa: Eq[F[A]]
//) extends FunSuite
//    with GeneratorDrivenPropertyChecks
//    with Discipline
//    with TestBase {
//
//  checkAll("validation", ExtruderErrorsTests[F].extruderErrors[A])
//
//  def missing(message: String): F[A]
//
//  test("validation missing")(forAll { message: String =>
//    ExtruderErrors[F].missing[A](message) <-> missing(message)
//  })
//}
