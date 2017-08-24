package extruder.examples

import extruder.core.{MapDecoder, MapHints, MapSource, Parameters, Validation}
import shapeless._
import shapeless.ops.hlist.Length
import shapeless.ops.nat._
import shapeless.syntax.sized._
import MapSource._

import cats.syntax.cartesian._

import scala.reflect.runtime.universe.TypeTag

case class CliBuilder[T]() {
  import CliBuilder._

  def build[ArgsRepr <: HList, DescrLen <: Nat](
    descriptions: Sized[List[(Name, String)], DescrLen]
  )(
    implicit argsRepr: LabelledGeneric.Aux[T, ArgsRepr],
    argsLen: Length.Aux[ArgsRepr, DescrLen],
    params: Parameters[T],
    hints: MapHints
  ): CliRunner[T, DescrLen] = {
    val argNames = params.eval(List.empty).map(x => hints.pathToString(x.path))
    CliRunner[T, ArgsLen, Props, PropsLen](argDescriptions, argNames, propsDescriptions)
  }
}

case class CliRunner[Args, ArgsLen <: Nat, Props, PropsLen <: Nat](
  argDescriptions: Sized[List[String], ArgsLen],
  argNames: List[String],
  propsDescriptions: Sized[List[String], PropsLen]
) {
  def run(argValues: List[String], propsValues: Map[String, String])(
    implicit argsDecoder: MapDecoder[Validation, Args],
    propsDecoder: MapDecoder[Validation, Props],
    toInt: ToInt[ArgsLen],
    tag: TypeTag[Props],
    hints: MapHints
  ): Option[Validation[(Args, Props)]] = {
    val propsName = tag.tpe.typeSymbol.name.toString
    val props = propsValues.map { case (k, v) => (hints.prependtoPath(propsName, k), v) }

    argValues
      .sized[ArgsLen]
      .map(x => (decode[Args](argNames.zip(x).toMap) |@| decode[Props](props)).map((_, _)))
  }
}

object CliBuilder {
  case class NoArgs()
  case class NoProps()

  sealed trait Name {
    def underlying: String
  }
  case class ArgName(underlying: String) extends AnyVal with Name
  case class PropName(underlying: String) extends AnyVal with Name

  def noProps[T]: CliBuilder[T, NoProps] = CliBuilder[T, NoProps]()
  def noArgs[V]: CliBuilder[NoArgs, V] = CliBuilder[NoArgs, V]()
}

object Test extends App {
  case class TrailingArgs(sd: String, sdf: Int)
  case class Props(sdfsdf: String, sdfs: Option[Long])

  println(
    CliBuilder[TrailingArgs, Props]()
      .build(Sized[List]("sdfsdf", "dsfs"), Sized[List]("dsfsd", "sdfsdfsf"))
      .run(List("sdfsd", "1"), Map("sdfsdf" -> "sdfsdf"))
      .get
  )
}
