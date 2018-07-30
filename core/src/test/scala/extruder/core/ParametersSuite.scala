package extruder.core

import cats.data.NonEmptyList
import org.scalatest.{Assertion, FunSuite}

class ParametersSuite extends FunSuite {
  import ParametersSuite._

  test("Can represent a simple case class") {
    test[Simple](
      List(List("Simple", "string"), List("Simple", "int")),
      List(None, None),
      List(true, true),
      List("String", "Int")
    )
  }

  test("Can represent a case class with defaults") {
    test[WithDefault](List(List("WithDefault", "default")), List(Some(default)), List(false), List("String"))
  }

  test("Can represent a case class with an optional parameter") {
    test[OptionalParam](List(List("OptionalParam", "opt")), List(None), List(false), List("String"))
  }

  test("Can represent a sealed family of types") {
    test[Sealed](
      List(List("type"), List("CCImpl", "d")),
      List(None, None),
      List(true, true),
      List("Double"),
      List(NonEmptyList.of("CCImpl", "ObjImpl"))
    )
  }

  test("Can represent a case class with a case class parameter") {
    test[Nested](
      List(List("Nested", "cc", "Simple", "string"), List("Nested", "cc", "Simple", "int")),
      List(None, None),
      List(true, true),
      List("String", "Int")
    )
  }

  test("Can represent a case class with an optional case class parameter") {
    test[NestedOpt](
      List(List("NestedOpt", "cc", "Simple", "string"), List("NestedOpt", "cc", "Simple", "int")),
      List(None, None),
      List(false, false),
      List("String", "Int")
    )
  }

  test("Can represent a case class with traversable parameters") {
    test[Traversables](
      List(List("Traversables", "list"), List("Traversables", "opt")),
      List(Some("one, two"), Some("1, 2")),
      List(false, false),
      List("List[String]", "List[Int]")
    )
  }

  def test[T](
    expectedNames: List[List[String]],
    expectedDefaults: List[Option[String]],
    expectedRequired: List[Boolean],
    expectedTypes: List[String] = List.empty,
    expectedUnionTypes: List[NonEmptyList[String]] = List.empty
  )(implicit params: Parameters[T]): Assertion = {
    val repr = params.eval(List.empty)
    assert(repr.map(_.path) === expectedNames)
    assert(repr.map(_.default) === expectedDefaults)
    assert(repr.map(_.required) === expectedRequired)
    assert(repr.flatMap {
      case x: StableRepr => Some(x.`type`)
      case _: Any => None
    } === expectedTypes)
    assert(repr.flatMap {
      case x: UnionRepr => Some(x.types)
      case _: Any => None
    } === expectedUnionTypes)
  }
}

object ParametersSuite {
  case class Simple(string: String, int: Int)

  val default: String = "default"
  case class WithDefault(default: String = default)

  case class OptionalParam(opt: Option[String])

  case class Nested(cc: Simple)

  case class NestedOpt(cc: Option[Simple])

  case class Traversables(list: List[String] = List("one", "two"), opt: Option[List[Int]] = Some(List(1, 2)))
}
