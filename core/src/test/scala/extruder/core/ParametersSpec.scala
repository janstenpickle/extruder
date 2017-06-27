package extruder.core

import cats.data.NonEmptyList
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class ParametersSpec extends Specification with ScalaCheck {
  import ParametersSpec._

  override def is: SpecStructure =
    s2"""
       Can represent a
        simple case class $simpleTest
        case class with defaults $defaultTest
        case class with an optional parameter $optionalTest
        sealed family of types $sealedTest
        case class with a case class parameter $nestedTest
        case class with an optional case class parameter $nestedOptionTest
        case class with traversable parameters $traversablesTest
      """

  def simpleTest: MatchResult[Any] = test[Simple](
    List(List("Simple", "string"), List("Simple", "int")),
    List(None, None),
    List(true, true),
    List("String", "Int")
  )

  def defaultTest: MatchResult[Any] =
    test[WithDefault](List(List("WithDefault", "default")), List(Some(default)), List(false), List("String"))

  def optionalTest: MatchResult[Any] =
    test[OptionalParam](List(List("OptionalParam", "opt")), List(None), List(false), List("String"))

  def sealedTest: MatchResult[Any] = test[Sealed](
    List(List("type"), List("CCImpl", "d")),
    List(None, None),
    List(true, true),
    List("Double"),
    List(NonEmptyList.of("CCImpl", "ObjImpl"))
  )

  def nestedTest: MatchResult[Any] = test[Nested](
    List(List("Nested", "cc", "Simple", "string"), List("Nested", "cc", "Simple", "int")),
    List(None, None),
    List(true, true),
    List("String", "Int")
  )

  def nestedOptionTest: MatchResult[Any] = test[NestedOpt](
    List(List("NestedOpt", "cc", "Simple", "string"), List("NestedOpt", "cc", "Simple", "int")),
    List(None, None),
    List(false, false),
    List("String", "Int")
  )

  def traversablesTest: MatchResult[Any] = test[Traversables](
    List(List("Traversables", "list"), List("Traversables", "opt")),
    List(Some("one, two"), Some("1, 2")),
    List(false, false),
    List("List[String]", "List[Int]")
  )

  def test[T](
    expectedNames: List[List[String]],
    expectedDefaults: List[Option[String]],
    expectedRequired: List[Boolean],
    expectedTypes: List[String] = List.empty,
    expectedUnionTypes: List[NonEmptyList[String]] = List.empty
  )(implicit params: Parameters[T]): MatchResult[Any] = {
    val repr = params.eval(Seq.empty)
    (repr.map(_.path) === expectedNames)
      .and(repr.map(_.default) === expectedDefaults)
      .and(repr.map(_.required) === expectedRequired)
      .and(repr.flatMap {
        case x: StableRepr => Some(x.`type`)
        case _: Any => None
      } === expectedTypes)
      .and(repr.flatMap {
        case x: UnionRepr => Some(x.types)
        case _: Any => None
      } === expectedUnionTypes)
  }
}

object ParametersSpec {
  case class Simple(string: String, int: Int)

  val default: String = "default"
  case class WithDefault(default: String = default)

  case class OptionalParam(opt: Option[String])

  case class Nested(cc: Simple)

  case class NestedOpt(cc: Option[Simple])

  case class Traversables(list: List[String] = List("one", "two"), opt: Option[List[Int]] = Some(List(1, 2)))
}
