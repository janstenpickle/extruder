package extruder.core

import cats.data.{Ior, Kleisli}
import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline.eq._
import cats.laws.discipline.{ExhaustiveCheck, FunctorTests}
import cats.{Eq, Id}
import extruder.CoreTestInstances._
import extruder.data.PathElement
import extruder.map.defaultSettings
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline

class TransformSuite extends AnyFunSuite with Discipline with ScalaCheckDrivenPropertyChecks {
  import TransformSuite._

  type Tran[A] = Transform[Id, Settings, Int, A]

  checkAll("Transform", FunctorTests[Tran].functor[Int, Int, Int])

  test("Can combine transform when input is left") {
    assert(
      Transform[Id, (Settings, Settings), Ior[String, String], Ior[String, String]]
        .run(List.empty, (settings, settings), Ior.Left("")) === Ior.Left("")
    )
  }

  test("Can combine transform when input is right") {
    assert(
      Transform[Id, (Settings, Settings), Ior[String, String], Ior[String, String]]
        .run(List.empty, (settings, settings), Ior.Right("")) === Ior.Right("")
    )
  }

  test("Can combine transform when input is both") {
    assert(
      Transform[Id, (Settings, Settings), Ior[String, String], Ior[String, String]]
        .run(List.empty, (settings, settings), Ior.Both("", "")) === Ior.Both("", "")
    )
  }

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

  test("Can create a transform from a Kleisli composition")(forAll { in: Int =>
    assert(
      Transform
        .fromKleisli(Kleisli[Id, (List[PathElement], Settings, Int), String](_._3.toString))
        .run(List.empty, defaultSettings, in) === in.toString
    )
  })
}

object TransformSuite {
  implicit def transformArb[A](implicit fin: Transform[Id, Settings, A, A]): Arbitrary[Transform[Id, Settings, A, A]] =
    Arbitrary(Gen.const(fin))

  implicit def transformEq[A: Eq: Arbitrary: ExhaustiveCheck]: Eq[Transform[Id, Settings, A, A]] =
    Eq.by[Transform[Id, Settings, A, A], A => Id[A]](tran => (a: A) => tran.run(List.empty, defaultSettings, a))

  val settings: Settings = new Settings {
    override def pathToString(path: List[String]): String = ???
  }
}
