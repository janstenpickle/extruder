package extruder.instances

import cats.instances.all._
import cats.laws.discipline.eq._
import cats.laws.discipline.{ContravariantTests, ExhaustiveCheck}
import cats.{Eq, Id}
import extruder.CoreTestInstances._
import extruder.core.{Encoder, Settings}
import extruder.map._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline

class EncoderInstancesSuite extends AnyFunSuite with Discipline {
  import EncoderInstancesSuite._

  type IdEncoder[A] = Encoder[Id, Settings, A, Map[String, String]]

  checkAll("EncoderT", ContravariantTests[IdEncoder].contravariant[Int, Int, Int])
}

object EncoderInstancesSuite {
  implicit def encoderTArb[A](
    implicit
    enc: Encoder[Id, Settings, A, Map[String, String]]
  ): Arbitrary[Encoder[Id, Settings, A, Map[String, String]]] =
    Arbitrary(Gen.const(enc))

  implicit def encoderTEq[A: Eq: Arbitrary: ExhaustiveCheck]: Eq[Encoder[Id, Settings, A, Map[String, String]]] =
    Eq.by[Encoder[Id, Settings, A, Map[String, String]], A => Map[String, String]](
      enc => (a: A) => enc.write(List.empty, defaultSettings, a)
    )
}
