package extruder.data

import cats.data.Kleisli
import cats.instances.int._
import cats.laws.discipline.FunctorTests
import cats.laws.discipline.eq._
import cats.{Eq, Id}
import extruder.core.Settings
import extruder.map.defaultSettings
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline

class TransformSuite extends FunSuite with Discipline with GeneratorDrivenPropertyChecks {
  import TransformSuite._

  type Prep[A] = Transform[Id, Settings, Int, A]

  checkAll("Prepare", FunctorTests[Prep].functor[Int, Int, Int])

  test("Can convert prepared data using by")(forAll { in: Int =>
    assert(
      Transform.by[Id, Settings, Int, Int, String](_.toString).run(List.empty, defaultSettings, in) === in.toString
    )
  })

  test("Can convert prepared data using byF")(forAll { in: Int =>
    assert(
      Transform.byF[Id, Settings, Int, Int, String](_.toString).run(List.empty, defaultSettings, in) === in.toString
    )
  })

  test("Can convert input data using inputBy")(forAll { in: Int =>
    assert(
      Transform.inputBy[Id, Settings, Int, String](_.toString).run(List.empty, defaultSettings, in) === in.toString
    )
  })

  test("Can convert input data using inputByF")(forAll { in: Int =>
    assert(
      Transform.inputByF[Id, Settings, Int, String](_.toString).run(List.empty, defaultSettings, in) === in.toString
    )
  })

  test("Can create a prepare from a Kleisli composition")(forAll { in: Int =>
    assert(
      Transform
        .fromKleisli(Kleisli[Id, (Settings, Int), String](_._2.toString))
        .run(List.empty, defaultSettings, in) === in.toString
    )
  })
}

object TransformSuite {
  implicit def prepArb[A](implicit fin: Transform[Id, Settings, A, A]): Arbitrary[Transform[Id, Settings, A, A]] =
    Arbitrary(Gen.const(fin))

  implicit def prepEq[A: Eq: Arbitrary]: Eq[Transform[Id, Settings, A, A]] =
    Eq.by[Transform[Id, Settings, A, A], A => Id[A]](prep => (a: A) => prep.run(List.empty, defaultSettings, a))
}
