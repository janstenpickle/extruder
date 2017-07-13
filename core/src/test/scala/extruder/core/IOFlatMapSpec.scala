package extruder.core

import cats.Eq
import cats.effect.IO
import cats.functor.Invariant
import cats.instances.all._
import cats.laws.discipline.CartesianTests.Isomorphisms
import cats.laws.discipline.FlatMapTests
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

abstract class IOFlatMapSpec[F[_], E](implicit IOF: IOFlatMap[F], A: ExtruderApplicativeError[F, E])
    extends Specification
    with Discipline {
  val tests: FlatMapTests[IOF[F, ?]] = FlatMapTests[IOF[F, ?]]

  implicit def IOFArb[A](implicit arb: Arbitrary[A]): Arbitrary[IO[F[A]]] =
    Arbitrary(
      Gen.oneOf(
        arb.arbitrary.map(a => IO(A.pure(a))),
        Gen
          .oneOf[F[A]](
            A.missing[A]("missing"),
            A.validationFailure[A]("fail"),
            A.validationException[A]("boom", new RuntimeException("exception"))
          )
          .map(IO.pure)
      )
    )

  implicit def IOFEq[A](implicit e: Eq[F[A]], ea: Eq[A]): Eq[IO[F[A]]] = TestCommon.ioEq(e)

  implicit def inv: Invariant[IOF[F, ?]] = new Invariant[IOF[F, ?]] {
    override def imap[A, B](fa: IOF[F, A])(f: (A) => B)(g: (B) => A): IOF[F, B] = fa.map(_.map(f).map(g).map(f))
  }

  implicit def iso: Isomorphisms[IOF[F, ?]] = Isomorphisms.invariant[IOF[F, ?]]

  implicit def feq[A](implicit e: Eq[A]): Eq[F[A]]

  override def is: SpecStructure = checkAll("IO FlatMap", tests.flatMap[Int, Int, Int])
}
