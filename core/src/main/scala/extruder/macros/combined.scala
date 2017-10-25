package extruder.macros

import extruder.core.{Decode, DecodeTypes}
import macrocompat.bundle

import scala.reflect.macros.whitebox


object combined {

}

class CombinedMacro(val c: whitebox.Context) {
  import c.universe._

  def decode[T : c.WeakTypeTag]: Tree = {
    val `type` = weakTypeOf[T]
    val typeSymbol = `type`.typeSymbol
  }
}

trait CombinedDecoders[T] {
  def apply(d1: Decode with DecodeTypes, d2: Decode with DecodeTypes)
}