package extruder.core

object FormatParameters {
  private val KeyCol = "Key"
  private val RequiredCol = "Required"
  private val TypeCol = "Type"
  private val DefaultCol = "Default"
  private val TypesCol = "Permitted Values"

  private def convertRequired(required: Boolean): String =
    if (required) "Y" else "N"

  private def colSep(length: Int): String =
    List.fill(length + 2)('-').mkString

  def maxLength(colName: String, selector: ParamRepr => String, params: List[ParamRepr]): Int =
    (colName.length :: params.map(p => selector(p).length)).max

  val typesToString: ParamRepr => String = {
    case union: UnionRepr => union.types.toList.mkString(" | ")
    case _: Any => ""
  }

  def asTable[T](pathToString: Seq[String] => String, namespace: Seq[String])(
    implicit parameters: Parameters[T]
  ): String = {
    val params = parameters.eval(namespace)
    val maxKeyLength = maxLength(KeyCol, p => pathToString(p.path), params)
    val maxRequiredLength = maxLength(RequiredCol, p => convertRequired(p.required), params)
    val maxTypeLength = maxLength(TypeCol, _.`type`, params)
    val maxDefaultLength = maxLength(DefaultCol, _.default.getOrElse(""), params)
    val maxAllowedValuesLength = maxLength(TypesCol, typesToString, params)

    val leftAlignFormat: String =
      s"| %-${maxKeyLength}s | %-${maxRequiredLength}s | %-${maxTypeLength}s | %-${maxDefaultLength}s | %-${maxAllowedValuesLength}s |%n"
    val separator: String =
      s"+${colSep(maxKeyLength)}+${colSep(maxRequiredLength)}+${colSep(maxTypeLength)}+${colSep(maxDefaultLength)}+${colSep(maxAllowedValuesLength)}+%n"

    val header = separator + leftAlignFormat.format(KeyCol, RequiredCol, TypeCol, DefaultCol, TypesCol) + separator
    val rows = params.foldLeft("") { (acc, p) =>
      acc + leftAlignFormat
        .format(pathToString(p.path), convertRequired(p.required), p.`type`, p.default.getOrElse(""), typesToString(p))
    }
    (header + rows + separator).format()
  }
}
