package extruder.effect

import cats.instances.all._
import cats.{Applicative, Eq}
import org.scalacheck.Arbitrary
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline

abstract class EffectSpec[F[_]](implicit F: Applicative[F]) extends Specification with ScalaCheck with Discipline {
  implicit val throwableEq: Eq[Throwable] = Eq.by(_.toString)
  implicit def feq[A](implicit eq: Eq[A]): Eq[F[A]]
  implicit def FArb[A](implicit arb: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(arb.arbitrary.map(a => F.pure(a)))
}
