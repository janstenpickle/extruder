package extruder.instances

import cats.Eq
import cats.instances.all._
import cats.laws.discipline.InvariantTests
import cats.syntax.applicative._
import extruder.core.{DecoderT, Settings}
import extruder.data.Validation
import extruder.map._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class DecoderTInstancesSuite extends FunSuite with Discipline {
  import DecoderTInstancesSuite._

  checkAll("DecoderT", InvariantTests[DecoderT[Validation, Settings, ?, Map[String, String]]].invariant[Int, Int, Int])
}

object DecoderTInstancesSuite {
  implicit def decoderTArb[A](
    implicit arb: Arbitrary[A]
  ): Arbitrary[DecoderT[Validation, Settings, A, Map[String, String]]] =
    Arbitrary(
      arb.arbitrary
        .map(a => DecoderT.make[Validation, Settings, A, Map[String, String]]((_, _, _, _) => a.pure[Validation]))
    )

  implicit def decoderTEq[A: Eq]: Eq[DecoderT[Validation, Settings, A, Map[String, String]]] =
    Eq.by(_.read(List.empty, defaultSettings, None, Map.empty))

}
