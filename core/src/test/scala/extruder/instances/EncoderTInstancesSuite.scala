package extruder.instances

import cats.instances.all._
import cats.laws.discipline.ContravariantTests
import cats.laws.discipline.eq._
import cats.{Eq, Id}
import extruder.core.{EncoderT, Settings}
import extruder.map._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class EncoderTInstancesSuite extends FunSuite with Discipline {
  import EncoderTInstancesSuite._

  type IdEncoder[A] = EncoderT[Id, Settings, A, Map[String, String]]

  checkAll("EncoderT", ContravariantTests[IdEncoder].contravariant[Int, Int, Int])
}

object EncoderTInstancesSuite {
  implicit def encoderTArb[A](
    implicit
    enc: EncoderT[Id, Settings, A, Map[String, String]]
  ): Arbitrary[EncoderT[Id, Settings, A, Map[String, String]]] =
    Arbitrary(Gen.const(enc))

  implicit def encoderTEq[A: Eq: Arbitrary]: Eq[EncoderT[Id, Settings, A, Map[String, String]]] =
    Eq.by[EncoderT[Id, Settings, A, Map[String, String]], A => Map[String, String]](
      enc => (a: A) => enc.write(List.empty, defaultSettings, a)
    )
}
